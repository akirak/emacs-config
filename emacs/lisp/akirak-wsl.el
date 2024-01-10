;;; akirak-wsl.el --- Support for Windows Subsystem for Linux -*- lexical-binding: t -*-

(defvar akirak-wsl-p nil)

;;;###autoload
(defun akirak-wsl-p ()
  "Return non-nil if the session is running inside Windows Subsystem for Linux."
  (pcase-exhaustive akirak-wsl-p
    (t t)
    (:no nil)
    (`nil (with-temp-buffer
            (when (ignore-errors (call-process "uname" nil (list t nil) nil "-a"))
              (goto-char (point-min))
              (let ((case-fold-search t))
                (if (re-search-forward "microsoft" nil t)
                    (setq akirak-wsl-p t)
                  (setq akirak-wsl-p :no)
                  nil)))))))

(provide 'akirak-wsl)
;;; akirak-wsl.el ends here
