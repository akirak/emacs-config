;;; akirak-subr.el --- Basic utilities -*- lexical-binding: t -*-

(defun akirak-major-mode-list ()
  "Return a list of major modes defined in the current Emacs instance."
  (let (modes)
    (cl-do-all-symbols
        (sym)
      (let ((name (symbol-name sym)))
        (when (and (commandp sym)
                   (string-suffix-p "-mode" name)
                   (let ((case-fold-search nil))
                     (string-match-p "^[a-z]" name))
                   (not (string-match-p (rx "/") name))
                   (not (string-match-p "global" name))
                   (not (memq sym minor-mode-list)))
          (push sym modes))))
    modes))

(defun akirak-complete-major-mode (prompt &optional initial history)
  (completing-read prompt (akirak-major-mode-list) nil t initial history))

(provide 'akirak-subr)
;;; akirak-subr.el ends here
