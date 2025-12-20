;;; akirak-flymake.el --- Flymake integration -*- lexical-binding: t -*-

(defun akirak-flymake-select-diagnostic (diags)
  (let* ((alist (mapcar (lambda (diag)
                          (cons (flymake-diagnostic-text diag)
                                diag))
                        diags))
         (text (completing-read "Select error: " alist nil t)))
    (cdr (assoc text alist))))

(defun akirak-flymake-filter-diags-by-pos (pos diags)
  (or (seq-filter (apply-partially (lambda (pos diag)
                                     (= pos (flymake--diag-beg diag)))
                                   pos)
                  diags)
      diags))

(provide 'akirak-flymake)
;;; akirak-flymake.el ends here
