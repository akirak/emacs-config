;;; akirak-custom.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-custom-erase (variable)
  "Erase customization of a variable saved to `custom-file'."
  (interactive (list (completing-read "Erase variable: "
                                      (akirak-custom-saved-variables))))
  (let ((symbol (cl-typecase variable
                  (symbol variable)
                  (string (intern variable)))))
    (when-let (standard-value (get symbol 'standard-value))
      (set symbol (eval (car standard-value))))
    (put symbol 'saved-value nil)
    (put symbol 'saved-variable-comment nil)
    (custom-save-all)))

(defun akirak-custom-saved-variables ()
  (with-current-buffer (find-file-noselect custom-file)
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "(custom-set-variables")
      (goto-char (match-beginning 0))
      (thread-last
        (read (current-buffer))
        cdr
        (mapcar #'cadr)))))

(provide 'akirak-custom)
;;; akirak-custom.el ends here
