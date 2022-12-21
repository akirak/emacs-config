;;; akirak-jsx.el ---  -*- lexical-binding: t -*-

(require 'treesit)
(require 'sgml-mode)

;;;###autoload
(defun akirak-jsx-close-tag ()
  (interactive)
  (let* ((initial (point))
         (ppss (syntax-ppss))
         (start (save-excursion
                  (goto-char (ppss-innermost-start ppss))
                  (treesit-node-end (treesit-node-at (point)))))
         (end (save-excursion
                (goto-char (ppss-innermost-start ppss))
                (condition-case _
                    (forward-sexp)
                  ;; forward-sexp may fail due to unmatching tags in JSX, so
                  ;; work around it
                  (error
                   (if-let (close (matching-paren (char-after)))
                       ;; This can produce a wrong result, but it seems to work
                       ;; in most cases
                       (search-forward (char-to-string close) nil t)
                     (error "No idea what to do"))))
                (backward-char)
                (treesit-node-start (treesit-node-at (point))))))
    (goto-char start)
    (when (re-search-forward (rx symbol-start "return" symbol-end)
                             end t)
      (setq start (point)))
    (let* ((orig (buffer-substring-no-properties start end))
           (str (with-temp-buffer
                  (insert orig)
                  (sgml-mode)
                  (goto-char (1+ (- initial start)))
                  (push-mark)
                  (sgml-close-tag)
                  (buffer-substring-no-properties (mark) (point)))))
      (goto-char initial)
      (insert str))))

(provide 'akirak-jsx)
;;; akirak-jsx.el ends here
