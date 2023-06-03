;;; akirak-org-dog.el ---  -*- lexical-binding: t -*-

(require 'org-dog)
(require 'akirak-org)
(require 'akirak-org-capture)

;;;###autoload
(defun akirak-org-dog-datetree-indirect-clock-in ()
  "Clock into a datetree in another file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "You must run this command in org-mode."))
  (require 'akirak-capture)
  (let* ((prop "ORG_DOG_PROJECTED_FILE")
         ;; TODO Completion
         (relative (org-entry-get nil prop t))
         (absolute (if relative
                       (oref (or (org-dog-find-file-object
                                  `(lambda (obj)
                                     (equal (file-name-sans-extension (oref obj relative))
                                            ,relative)))
                                 (user-error "File %s is not known. Set %s again"
                                             relative prop))
                             absolute)
                     (org-dog-complete-file (format "Set a new value of %s: " prop))))
         (_ (unless relative
              (org-entry-put nil prop
                             (file-name-sans-extension
                              (oref (org-dog-file-object absolute) relative)))))
         (chapter (org-link-display-format
                   (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (replace-regexp-in-string
                      org-list-full-item-re
                      ""
                      (string-chop-newline (thing-at-point 'line))))))
         (title (org-link-display-format
                 (org-get-heading t t t t)))
         (first-link (save-excursion
                       (org-back-to-heading)
                       (save-match-data
                         (when (re-search-forward org-link-any-re (org-entry-end-position)
                                                  t)
                           (match-string 0)))))
         (filetags (buffer-local-value 'org-file-tags
                                       (or (find-buffer-visiting absolute)
                                           (find-file-noselect absolute))))
         (template (akirak-org-capture-make-entry-body
                     (if chapter
                         (concat chapter " - " title)
                       title)
                     :todo "UNDERWAY"
                     :tags (seq-difference (org-get-tags)
                                           filetags
                                           #'equal)
                     :body (concat ":METADATA:\n"
                                   "Context: %a\n"
                                   (if first-link
                                       (concat "Source: " first-link "\n")
                                     "")
                                   ":END:")))
         (org-capture-entry (car (doct
                                  `(("Datetree"
                                     :keys "x"
                                     :file ,absolute
                                     :function akirak-capture--goto-backlog
                                     :clock-in t :clock-resume t
                                     :template ,template))))))
    (org-capture)))

(defun akirak-org-dog-context-files (type &optional deep)
  (pcase (org-dog-context-edge type)
    (`(,_ . ,ctx)
     (when ctx
       (let (files)
         (dolist (file-obj (org-dog-context-file-objects ctx))
           (let ((fpath (oref file-obj absolute)))
             (if deep
                 (setq files (if deep
                                 (thread-last
                                   (org-dog-overview-scan (list fpath) :fast t)
                                   (mapcar #'car)
                                   (append files))))
               (push fpath files))))
         (cl-remove-duplicates (nreverse files)
                               :test #'equal))))))

(defun akirak-org-dog-major-mode-files ()
  (akirak-org-dog-context-files 'major-mode t))

(defun akirak-org-dog-project-files ()
  (akirak-org-dog-context-files 'project t))

(defun akirak-org-dog-path-files ()
  (akirak-org-dog-context-files 'path t))

(defun akirak-org-dog-language-files ()
  (akirak-org-dog-context-files 'language t))

(defun akirak-org-dog-org-tags-files ()
  (akirak-org-dog-context-files 'org-tags t))

;;;; In-file capture functions

;;;###autoload
(defun akirak-org-dog-add-to-index (begin end)
  "Add a link target to Index section in the file."
  (interactive (pcase (akirak-org-dog--target-bounds)
                 (`(,begin . ,end) (list begin end))))
  (let* ((buffer (org-base-buffer (current-buffer)))
         (candidates (with-current-buffer buffer
                       (org-with-wide-buffer
                        (goto-char (point-min))
                        (when (re-search-forward (format org-complex-heading-regexp-format
                                                         "Index")
                                                 nil t)
                          (org-narrow-to-subtree)
                          (org-map-entries
                           (lambda ()
                             (let ((olp (org-get-outline-path t t)))
                               (cons (org-format-outline-path olp nil nil "/")
                                     olp))))))))
         (source (buffer-substring-no-properties begin end))
         (dest (completing-read (format "Define \"%s\" in an entry: "
                                        source)
                                (or candidates '("Index"))))
         (olp (thread-last
                (split-string dest "/")
                (cl-remove-if #'string-empty-p)))
         (radiop (yes-or-no-p "Radio target? "))
         (target (if radiop
                     source
                   (read-string "Exact name of the target: " source))))
    (org-with-wide-buffer
     (goto-char (point-min))
     (akirak-org-goto-or-create-olp olp)
     (if (re-search-forward (org-item-re)
                            (org-entry-end-position)
                            t)
         (org-end-of-item-list)
       (org-end-of-meta-data t))
     (insert (if (bolp) "" "\n")
             "- "
             (if radiop
                 (format "<<<%s>>>" target)
               (format "<<%s>>" target))
             "\n")
     ;; Restarting font-lock-mode resets org-capture buffers to
     ;; fundamental-mode, which is annoying
     ;;
     ;; (when radiop
     ;;   (org-update-radio-target-regexp))
     )
    (unless radiop
      (delete-region begin end)
      (insert (org-link-make-string target
                                    (unless (equal target source)
                                      source))))))

(defun akirak-org-dog--target-bounds ()
  (cond
   ;; Use the region
   ((use-region-p)
    (car (region-bounds)))
   ;; Create a bracket link.
   ((thing-at-point-looking-at org-link-bracket-re)
    (cons (match-beginning 1) (match-end 1)))
   ;; With a positive numeric prefix, select N words from the
   ;; point.
   ((and (numberp current-prefix-arg)
         (> current-prefix-arg 0))
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (forward-word current-prefix-arg)
        (cons (car bounds) (point)))))
   ;; With a negative numeric prefix, select N words till
   ;; the point.
   ((and (numberp current-prefix-arg)
         (< current-prefix-arg 0))
    (save-excursion
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (forward-word current-prefix-arg)
        (cons (point) (cdr bounds)))))
   ;; Use the symbol at point.
   (t
    (bounds-of-thing-at-point 'symbol))))

;;;; Updating

(defcustom akirak-org-dog-auto-update-hook
  '(akirak-org-dog-update-auto-babel-blocks)
  ""
  :type 'hook)

;;;###autoload
(defun akirak-org-dog-auto-update-hook ()
  (run-hooks 'akirak-org-dog-auto-update-hook))

;;;###autoload
(defun akirak-org-dog-update-auto-babel-blocks ()
  "Update babel blocks named \"auto-update\" in each file."
  (interactive)
  (let (files)
    (dolist (file (org-agenda-files))
      (with-current-buffer (or (find-buffer-visiting file)
                               (find-file-noselect file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((bound (save-excursion (re-search-forward org-heading-regexp nil t))))
           (when bound
             (narrow-to-region (point-min) bound)
             (when-let (pos (org-babel-find-named-block "auto-update"))
               (goto-char pos)
               (message "Executing a babel block at %d in %s" pos file)
               (org-babel-execute-src-block)
               (push file files)))))))
    (when files
      (message "Executed source blocks in %s"
               (mapconcat #'abbreviate-file-name files " ")))
    ;; Reload data from the blocks
    (org-dog-reload-files)))

;;;###autoload
(defun akirak-org-dog-to-plan ()
  "Search headings that do not have corresponding org-memento rules."
  (interactive)
  (require 'org-memento)
  (require 'org-memento-policy)
  (org-memento-policy-maybe-load)
  (let ((file (or (when (derived-mode-p 'org-mode)
                    (buffer-file-name (org-base-buffer (current-buffer))))
                  (bound-and-true-p org-ql-view-buffers-files)
                  (user-error "Not in org-mode")))
        (files (gensym)))
    (set files (thread-last
                 (org-memento-policy-rules
                  :start-date (format-time-string "%F")
                  :span 'week)
                 (seq-filter #'org-memento-yield-instance-p)
                 (mapcar #'org-memento-group-path)
                 (seq-uniq)
                 (mapcar #'org-memento-group-agenda-files)
                 (mapcar #'car)
                 (delq nil)
                 (seq-uniq)))
    (org-ql-search file
      `(and (todo "UNDERWAY")
            (when-let* ((headline (org-get-heading t t t t))
                        (link (when (string-match org-link-bracket-re headline)
                                (match-string 1 headline)))
                        (rel (when (string-prefix-p "org-dog:" link)
                               (string-remove-prefix "org-dog:" link)))
                        (file (org-dog-resolve-relative-file rel)))
              (not (member file ,files))))
      :super-groups '((:auto-parent t)))))

(provide 'akirak-org-dog)
;;; akirak-org-dog.el ends here
