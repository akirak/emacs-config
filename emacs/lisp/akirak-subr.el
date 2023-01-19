;;; akirak-subr.el --- Basic utilities -*- lexical-binding: t -*-

(defvar org-src-lang-modes)

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

;;;###autoload
(cl-defun akirak-complete-major-mode (prompt &optional initial history
                                             &key org-src-langs)
  (completing-read prompt (append (when org-src-langs
                                    (require 'org-src)
                                    (mapcar #'car org-src-lang-modes))
                                  (akirak-major-mode-list))
                   nil t initial history))

(provide 'akirak-subr)
;;; akirak-subr.el ends here
