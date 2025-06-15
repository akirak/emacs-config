;;; akirak-flymake.el --- Flymake integration -*- lexical-binding: t -*-

(defun akirak-flymake-select-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

(provide 'akirak-flymake)
;;; akirak-flymake.el ends here
