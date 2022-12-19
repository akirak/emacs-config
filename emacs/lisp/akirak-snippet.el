;;; akirak-snippet.el --- Snippet management -*- lexical-binding: t -*-

(require 'akirak-org-capture)
(require 'akirak-org-dog)
(require 'org-ml)
(require 'org-dog-context)

(cl-defstruct akirak-snippet-entry
  name filename language description args body olp)

(defcustom akirak-snippet-capture-target
  #'akirak-snippet-select-location
  ""
  :type 'function)

(defcustom akirak-snippet-olp-filter-regexp
  (rx bos (or digit "Mnemonics" "Snippets"))
  "Regexp for headings that should not be displayed in completion."
  :type 'regexp)

(defvar akirak-snippet-file-cache (make-hash-table :test #'equal))

(defvar akirak-snippet-entries-with-context nil
  "A cons cell of files and snippet entries.

This is also useful for completion.")

;;;###autoload
(defun akirak-snippet-search ()
  "Search a snippet and expand it."
  (interactive)
  (let* ((entries (akirak-snippet--search))
         (name (completing-read "Snippet: " #'akirak-snippet--completions)))
    (when-let (entry (cdr (assoc name entries)))
      (akirak-snippet--expand entry))))

(defun akirak-snippet--completions (string pred action)
  (if (eq action 'metadata)
      '(metadata . ((category . snippet)
                    (group-function . akirak-snippet--group)
                    (annotation-function . akirak-snippet--annotator)))
    (complete-with-action action (cdr akirak-snippet-entries-with-context) string pred)))

(defun akirak-snippet--annotator (name)
  (when-let (entry (cdr (assoc name (cdr akirak-snippet-entries-with-context))))
    (let ((description (akirak-snippet-entry-description entry))
          (olp (akirak-snippet--filter-olp (akirak-snippet-entry-olp entry))))
      (when (or description olp)
        (concat " "
                (when description
                  (propertize description 'face 'font-lock-comment-face))
                (when olp
                  (propertize (format " (%s)"
                                      (thread-first
                                        olp
                                        (nreverse)
                                        (org-format-outline-path nil nil " < ")))
                              'face 'font-lock-comment-face)))))))

(defun akirak-snippet--filter-olp (olp)
  (cl-remove-if (lambda (s)
                  (string-match-p akirak-snippet-olp-filter-regexp s))
                olp))

(defun akirak-snippet--group (completion transform)
  (let ((filename (thread-last
                    (cdr (assoc completion (cdr akirak-snippet-entries-with-context)))
                    (akirak-snippet-entry-filename)
                    (file-name-nondirectory))))
    (if transform
        completion
      filename)))

(defun akirak-snippet--org-files ()
  (append (akirak-org-dog-path-files)
          (akirak-org-dog-major-mode-files)))

(defun akirak-snippet--search ()
  (if-let (files (akirak-snippet--org-files))
      (if (equal files (car akirak-snippet-entries-with-context))
          (cdr akirak-snippet-entries-with-context)
        (let ((entries (thread-last
                         (mapcar #'akirak-snippet--load files)
                         (apply #'append)
                         (mapcar (lambda (entry)
                                   (cons (akirak-snippet-entry-name entry)
                                         entry))))))
          ;; Cache the entries to reduce list operations
          (setq akirak-snippet-entries-with-context (cons files entries))
          entries))
    (user-error "No files")))

(defun akirak-snippet--expand (entry)
  (let ((pre (plist-get (akirak-snippet-entry-args entry) :pre))
        (post (plist-get (akirak-snippet-entry-args entry) :post))
        (no-save-mark (plist-get (akirak-snippet-entry-args entry) :no-save-mark))
        (body (string-trim-right (akirak-snippet-entry-body entry)
                                 "[\n\r]+")))
    ;; Push the marker so the user can return to the original position after
    ;; expansion.
    (unless no-save-mark
      (push-mark))
    (atomic-change-group
      (when (and pre (listp pre))
        (eval pre))
      (cl-case (plist-get (akirak-snippet-entry-args entry) :snippet)
        ((yasnippet yas) (progn
                           (require 'yasnippet)
                           (yas-minor-mode 1)
                           (yas-expand-snippet body)))
        (tempo (akirak-snippet--insert-tempo (read body) nil))
        (skeleton (skeleton-insert (read body)))
        (command (let ((command (eval (read body))))
                   (call-interactively command)))
        (otherwise (insert body)))
      (when (and post (listp post))
        (eval post)))))

(defun akirak-snippet-reload ()
  "Reload snippets from the current file."
  (interactive)
  (setq akirak-snippet-entries-with-context nil)
  (akirak-snippet--load (buffer-file-name) t))

(defun akirak-snippet-invalidate (file)
  "Reload snippets from the current file."
  (interactive)
  (setq akirak-snippet-entries-with-context nil)
  (remhash (expand-file-name file) akirak-snippet-file-cache))

(defun akirak-snippet-clear ()
  "Clear all snippet cache."
  (interactive)
  (setq akirak-snippet-entries-with-context nil)
  (clrhash akirak-snippet-file-cache))

(defun akirak-snippet--load (file &optional force)
  (let* ((file (expand-file-name file))
         (result (gethash file akirak-snippet-file-cache :missing)))
    (when (or force (eq result :missing))
      (setq result
            (thread-last
              (org-ql-select file
                '(tags "@snippet")
                :action
                `(org-with-wide-buffer
                  (narrow-to-region (point) (org-entry-end-position))
                  (org-match-line org-complex-heading-regexp)
                  (let ((name (match-string-no-properties 4))
                        description)
                    (org-end-of-meta-data t)
                    (org-skip-whitespace)
                    (unless (or (org-at-keyword-p)
                                (org-at-block-p))
                      (setq description (thing-at-point 'sentence t)))
                    (akirak-snippet--next-block :file ,file
                                                :name name
                                                :description description))))
              (delq nil)))
      (puthash file result akirak-snippet-file-cache))
    result))

(cl-defun akirak-snippet--next-block (&key file name description)
  (when (re-search-forward org-babel-src-block-regexp nil t)
    (goto-char (match-beginning 0))
    (let* ((element (org-element-at-point))
           (language (org-element-property :language element))
           (args (read (format "(%s)" (org-element-property :parameters element))))
           (body (akirak-snippet--unindent
                  (org-element-property :value element))))
      (goto-char (org-element-property :end element))
      (make-akirak-snippet-entry
       :description description
       :language language
       :filename file
       :olp (org-get-outline-path)
       :name (or (when-let (name (plist-get args :name))
                   (pcase name
                     ;; When 'literal is given as the :name property,
                     ;; the literal content will be used as the name.
                     (`literal
                      (thread-first
                        (replace-regexp-in-string "\n" "" (string-trim body))
                        (akirak-snippet--substring 0 50)
                        (propertize 'face 'font-lock-string-face)))
                     ((pred symbolp)
                      (symbol-name name))
                     ;; string
                     (t
                      name)))
                 name)
       :args args
       :body body))))

(defun akirak-snippet--substring (string start end)
  (if (> (length string) end)
      (substring string start)
    string))

(defun akirak-snippet-try ()
  "Try out the snippet block at point."
  (interactive)
  (when-let (entry (akirak-snippet--next-block))
    (with-current-buffer (get-buffer-create "*try snippet*")
      (erase-buffer)
      (pop-to-buffer (current-buffer))
      (akirak-snippet--expand entry))))

(defun akirak-snippet--unindent (string)
  (save-match-data
    (if (string-match (rx bol (+ blank)) string)
        (replace-regexp-in-string (concat "^" (match-string 0 string))
                                  "" string)
      string)))

;;;###autoload
(defun akirak-snippet-capture (begin end &optional file)
  "Capture the region as a snippet."
  (interactive "r")
  (akirak-snippet--capture
   :file file
   :language (string-remove-suffix "-mode" (symbol-name major-mode))
   :parameters '(:snippet plain)
   :src (buffer-substring-no-properties begin end)))

;;;###autoload
(defun akirak-snippet-capture-literal (begin end &optional file)
  (interactive "r")
  (akirak-snippet--capture
   :file file
   :language (string-remove-suffix "-mode" (symbol-name major-mode))
   :parameters '(:snippet plain :name literal)
   :src (buffer-substring-no-properties begin end)))

;;;###autoload
(defun akirak-snippet-capture-tempo (begin end &optional file)
  "Capture the region as a tempo snippet.

See `tempo-define-template' for the syntax of elements in the
template."
  (interactive "r")
  (akirak-snippet--capture
   :file file
   :language "lisp-data"
   :parameters '(:snippet tempo)
   :src (akirak-snippet--to-tempo
         (buffer-substring-no-properties begin end))
   :original-language (string-remove-suffix "-mode" (symbol-name major-mode))
   :original-src (buffer-substring-no-properties begin end)))

(defun akirak-snippet--to-tempo (src)
  (with-temp-buffer
    (lisp-data-mode)
    (let ((initial t)
          (prev-indent 0)
          (indent 0))
      (insert "(")
      (dolist (s (split-string (string-trim-right src) "\n"))
        (when (string-match (rx bol (* blank)) s)
          (setq indent (match-end 0)))
        (if initial
            (setq initial nil)
          (if (> indent prev-indent)
              (insert "\nn> ")
            (insert "\nn ")))
        (setq prev-indent indent)
        (insert (prin1-to-string (string-trim s))))
      (insert ")"))
    (indent-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun akirak-snippet-save-as-tempo (begin end)
  "Save the region as a tempo template."
  (interactive "r")
  (kill-new (akirak-snippet--to-tempo
             (buffer-substring-no-properties begin end))))

(cl-defun akirak-snippet--capture (&key language file parameters src
                                        original-language original-src)
  (let ((org-capture-entry
         (car (doct `(("Snippet"
                       :keys "s"
                       :file ,(or file
                                  (completing-read "File: " (akirak-snippet--org-files)))
                       :function ,akirak-snippet-capture-target
                       :template
                       ("* %^{title} :@snippet:"
                        ,akirak-org-capture-default-drawer
                        "%?"
                        ,(thread-last
                           (org-ml-build-src-block
                            :language language
                            :parameters parameters
                            :value src)
                           (org-ml-to-trimmed-string))
                        ,@(when original-src
                            `(""
                              "Here is an example:"
                              ,(thread-last
                                 (org-ml-build-src-block
                                  :language original-language
                                  :value original-src)
                                 (org-ml-to-trimmed-string))))
                        ,@(when-let
                              (url (ignore-errors
                                     (magit-git-string "config" "--local" "remote.origin.url")))
                            (list "Origin: " url)))))))))
    (add-hook 'org-capture-after-finalize-hook
              #'akirak-snippet--after-capture-finalize)
    (org-capture)))

(defun akirak-snippet--after-capture-finalize ()
  (akirak-snippet-clear)
  (remove-hook 'org-capture-after-finalize-hook
               #'akirak-snippet--after-capture-finalize))

(defun akirak-snippet-goto-section ()
  (if-let (marker (ignore-errors
                    (org-find-olp '("Snippets") t)))
      (org-goto-marker-or-bmk marker)
    (cond
     ((re-search-forward (rx bol "*" (+ blank) "Resources") nil t)
      (end-of-line 0))
     ((re-search-forward (rx bol "#" (+ blank) "Local Variables:") nil t)
      (org-back-to-heading)
      (end-of-line 0))
     (t
      (goto-char (point-max))))
    (insert "\n* Snippets :@snippet:")))

(defun akirak-snippet-select-location ()
  (let ((org-refile-targets (list (cons (buffer-file-name)
                                        '(:maxlevel . 99))))
        (org-refile-target-verify-function nil))
    (goto-char (nth 3 (org-refile-get-location "Parent of the snippet entry: " nil t)))))

(defun akirak-snippet--insert-tempo (elements &optional on-region)
  "Insert a tempo template.

For ELEMENTS, see the documentation for `tempo-define-template'.

If ON-REGION is non-nil, the `r' elements are replaced with the
current region."
  (require 'tempo)
  (let ((sym (gensym "my-tempo-template")))
    (set sym elements)
    (unwind-protect
        (tempo-insert-template sym on-region)
      (unintern (symbol-name sym) obarray))))

(provide 'akirak-snippet)
;;; akirak-snippet.el ends here
