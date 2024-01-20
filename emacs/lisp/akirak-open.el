;;; akirak-open.el ---  -*- lexical-binding: t -*-

(defvar akirak-open-default-command nil)

;;;###autoload
(defun akirak-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "f")
  (when (file-remote-p file)
    (user-error "Remote file is not supported"))
  (let ((file (convert-standard-filename (expand-file-name file))))
    (message "Opening %s externally" file)
    (akirak-open-default file)))

;;;###autoload
(defun akirak-open-default (file-or-url &optional _)
  "Open FILE-OR-URL with the default application."
  (with-current-buffer (generate-new-buffer "*embark open*")
    (pcase (if (and akirak-open-default-command
                    (or (file-exists-p (car akirak-open-default-command))
                        (executable-find (car akirak-open-default-command))))
               akirak-open-default-command
             (akirak-open-default-command))
      (`(,program . ,options)
       (apply #'call-process program nil t nil (append options (list file-or-url)))))))

(defun akirak-open-default-command ()
  "Return the default program with args for opening a file.

This function also sets `akirak-open-default-command' variable to
the returned value to memorize the result."
  (setq akirak-open-default-command
        (ensure-list (pcase system-type
                       (`darwin "open")
                       (`gnu/linux
                        (or (when (akirak-wsl-p)
                              (or (executable-find "wslview")
                                  (executable-find "wsl-open")))
                            (when-let (exe (executable-find "handlr"))
                              (list exe "open"))
                            (executable-find "xdg-open")))))))

;;;###autoload (autoload 'akirak-open-can-use-default-program "akirak-open")
(defalias 'akirak-open-can-use-default-program #'akirak-open-default-command)

(provide 'akirak-open)
;;; akirak-open.el ends here
