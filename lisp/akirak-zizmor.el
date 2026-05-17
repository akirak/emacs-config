;;; akirak-zizmor.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-zizmor-on-file (&optional arg)
  "Run zizmor on the current file.

With a prefix argument, fix the issues."
  (interactive "P" yaml-mode)
  (compile (format "zizmor %s %s"
                   (if arg
                       "--fix"
                     "")
                   (file-name-nondirectory (buffer-file-name)))))

(provide 'akirak-zizmor)
;;; akirak-zizmor.el ends here
