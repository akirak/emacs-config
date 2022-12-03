;;; akirak-misc.el ---  -*- lexical-binding: t -*-

(defun akirak-misc-sentence-lines (text)
  (cl-flet
      ((split-sentences (str)
         (let (sentences)
           (with-temp-buffer
             (insert str)
             (replace-regexp-in-region (rx (* blank) "\n"
                                           (* blank))
                                       " "
                                       (point-min) (point-max))
             (goto-char (point-min))
             (let ((start (point)))
               (while (ignore-errors (forward-sentence))
                 (push (string-trim (buffer-substring-no-properties start (point)))
                       sentences)
                 (setq start (point)))))
           sentences)))
    (thread-last
      (split-string text "\n\n")
      (cl-remove-if #'string-empty-p)
      (mapcar #'split-sentences)
      (apply #'append)
      (cl-remove-if #'string-empty-p)
      (reverse))))

;;;###autoload
(defun akirak-misc-sentence-lines-on-region (begin end)
  (interactive "r")
  (let ((string (buffer-substring-no-properties begin end)))
    (delete-region begin end)
    (insert (string-join (akirak-misc-sentence-lines string) "\n"))))

(provide 'akirak-misc)
;;; akirak-misc.el ends here
