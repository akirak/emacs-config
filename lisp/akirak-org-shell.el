;;; akirak-org-shell.el --- Terminal integration for Org mode -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'akirak-org)
(require 'akirak-transient)

(defclass akirak-org-shell-buffer-variable (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-org-shell-buffer-variable))
  (akirak-org-shell--read-buffer (oref obj prompt)
                                 (oref obj value)))

(cl-defmethod transient-format-value ((obj akirak-org-shell-buffer-variable))
  (let* ((value (oref obj value))
         (buffer (when value
                   (get-buffer value))))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if (and buffer
              (buffer-live-p buffer))
         (propertize (buffer-name buffer) 'face 'transient-value)
       (propertize "not set" 'face 'transient-inactive-value))
     (propertize ")" 'face 'transient-inactive-value))))

(defvar-local akirak-org-shell-buffer nil)

(transient-define-infix akirak-org-shell-buffer-infix ()
  :class 'akirak-org-shell-buffer-variable
  :variable 'akirak-org-shell-buffer
  :prompt "Terminal buffer: "
  :description "Buffer")

(defun akirak-org-shell--buffer-live-p ()
  (and akirak-org-shell-buffer
       (buffer-live-p akirak-org-shell-buffer)))

;;;###autoload (autoload 'akirak-org-shell-transient "akirak-org-shell" nil 'interactive)
(transient-define-prefix akirak-org-shell-transient ()
  :refresh-suffixes t
  ["Options"
   ("-b" akirak-org-shell-buffer-infix)]
  ["Insert"
   :if akirak-org-shell--buffer-live-p
   ("y" "Insert response" akirak-org-shell-insert-response)]
  ["Send"
   :if akirak-org-shell--buffer-live-p
   ("r" "Send region as Markdown" akirak-org-shell-send-region-as-markdown
    :if use-region-p)
   ("<C-return>" "Send the current Org entry"
    akirak-org-shell-send-org-entry-as-markdown)
   ("RET" "Send any command" akirak-org-shell-send-command)]
  (interactive nil org-mode)
  (unless akirak-org-shell-buffer
    (setq akirak-org-shell-buffer
          (akirak-org-shell--read-buffer "Terminal buffer: " akirak-org-shell-buffer)))
  (transient-setup 'akirak-org-shell-transient))

(defun akirak-org-shell--read-buffer (prompt default)
  (let ((buffer-name (read-buffer prompt default nil #'akirak-shell-buffer-p)))
    (or (get-buffer buffer-name)
        (akirak-shell-eat-new :dir (akirak-org-shell--read-directory)
                              :name buffer-name
                              :noselect t))))

(defun akirak-org-shell--read-directory ()
  (let* ((header-args (thread-first
                        (org-entry-get nil "header-args" t)
                        (org-babel-parse-header-arguments 'no-eval)))
         (default (alist-get :dir header-args))
         (dir (read-directory-name "Directory: " default nil t)))
    (unless (and default
                 (file-equal-p default dir))
      (let ((new-header-args (org-babel-combine-header-arg-lists
                              header-args
                              `((:dir . ,(abbreviate-file-name dir))))))
        (org-entry-put nil "header-args"
                       (mapconcat (lambda (pair)
                                    (concat (symbol-name (car pair))
                                            " " (cdr pair)))
                                  new-header-args " "))))
    dir))

;;;###autoload
(defun akirak-org-shell-set-directory ()
  "Set :dir property of the current Org entry."
  (interactive)
  (akirak-org-shell--read-directory))

(defun akirak-org-shell-directory ()
  "Return :dir property in the current header args."
  (let ((header-args (thread-first
                       (org-entry-get nil "header-args" t)
                       (org-babel-parse-header-arguments 'no-eval))))
    (alist-get :dir header-args)))

(defun akirak-org-shell-insert-response (n)
  (interactive "P" org-mode)
  (akirak-shell-insert-response-to-org akirak-org-shell-buffer (or n 1)))

(defun akirak-org-shell--send-string (string)
  (akirak-shell-send-string-to-buffer (or akirak-org-shell-buffer
                                          (error "Not set the buffer akirak-org-shell-buffer"))
    string
    :confirm t))

(defun akirak-org-shell-send-region-as-markdown ()
  (interactive nil org-mode)
  (if (use-region-p)
      (thread-first
        (buffer-substring (region-beginning) (region-end))
        (akirak-org-shell--convert-to-gfm)
        (akirak-org-shell--send-string))
    (user-error "Not selecting a region")))

(defun akirak-org-shell-send-org-entry-as-markdown ()
  (interactive nil org-mode)
  (thread-first
    (save-excursion
      (goto-char (org-entry-beginning-position))
      (org-end-of-meta-data t)
      (buffer-substring (point) (org-entry-end-position)))
    (akirak-org-shell--convert-to-gfm)
    (akirak-org-shell--send-string)))

(defun akirak-org-shell-send-block ()
  (interactive nil org-mode)
  (pcase (org-babel-get-src-block-info)
    (`(,_lang ,body . ,_)
     (thread-first
       (string-chop-newline body)
       (akirak-org-shell--send-string)))))

(defun akirak-org-shell-send-command (input)
  (interactive "sInput: " org-mode)
  (akirak-org-shell--send-string input))

(defun akirak-org-shell--convert-to-gfm (string)
  (require 'akirak-pandoc)
  (akirak-pandoc-convert-string string :from "org" :to "gfm"))

(defun akirak-org-shell-convert-to-relative-paths (beg end)
  "Convert paths in the region to relative path from the directory."
  (interactive "r")
  (let ((base-dir (akirak-org-shell-directory)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward (rx symbol-start (any "~/") (+ nonl) word-end) end t)
        (let ((path (match-string 0)))
          (when (string-prefix-p base-dir (abbreviate-file-name path))
            (replace-match (file-relative-name path base-dir))))))))

(defun akirak-org-shell-aider-copy ()
  "Insert the last message in Aider into the current point."
  (interactive)
  (akirak-org-shell--send-string "/copy")
  (sit-for 0.1)
  (let ((pos (point)))
    (yank)
    (akirak-pandoc-replace-with-org pos (point))
    (pulse-momentary-highlight-region pos (point))))

(provide 'akirak-org-shell)
;;; akirak-org-shell.el ends here
