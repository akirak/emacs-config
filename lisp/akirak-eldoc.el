;;; akirak-eldoc.el --- Eldoc support -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-eldoc-show-buffer (&optional arg)
  "Display eldoc in a window."
  (interactive "P")
  (pcase arg
    ('(16)
     (dolist (w (window-list))
       (let ((buffer (window-buffer w)))
         (when (or (eq (buffer-local-value 'major-mode buffer) 'help-mode)
                   (equal (buffer-name buffer) (buffer-name (eldoc-doc-buffer))))
           (quit-window nil w)))))
    ('(4)
     (unless (get-buffer-window (eldoc-doc-buffer))
       (eldoc))
     (akirak-eldoc--to-help-buffer))
    (_
     (eldoc-doc-buffer t))))

(defun akirak-eldoc--to-help-buffer ()
  (let* ((thing (thing-at-point 'symbol 'no-properties))
         (bookmark (cons thing (funcall bookmark-make-record-function))))
    (help-setup-xref (list 'akirak-eldoc--help-redo-helper (point) (current-buffer))
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (insert (format "%s [" thing))
      (insert-text-button "source"
                          'action `(lambda (_) (bookmark-jump ',bookmark))
                          'follow-link t
                          'help-echo "Visit the original location")
      (insert "]\n"
              (with-current-buffer (eldoc-doc-buffer)
                (buffer-string))))))

(defun akirak-eldoc--help-redo-helper (pos buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (eldoc)
      (akirak-eldoc--to-help-buffer nil))))

(provide 'akirak-eldoc)
;;; akirak-eldoc.el ends here
