;;; akirak-open.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "f")
  (when (file-remote-p file)
    (user-error "Remote file is not supported"))
  (let ((file (convert-standard-filename (expand-file-name file))))
    (message "Opening %s externally" file)
    (akirak-open-default file)))

(defun akirak-open-default (file-or-url &optional _)
  "Open FILE-OR-URL with the default application."
  (with-current-buffer (generate-new-buffer "*embark open*")
    (pcase system-type
      (`darwin
       (call-process "open" nil t nil file-or-url))
      (`gnu/linux
       (ensure-list
        (or (when (akirak-wsl-p)
              (when-let (exe (or (executable-find "wslview")
                                 (executable-find "wsl-open")))
                (call-process exe nil t nil file-or-url)))
            (when-let (exe (executable-find "handlr"))
              (call-process exe nil t nil "open" file-or-url))
            (when-let (exe (executable-find "xdg-open"))
              (call-process exe nil t nil file-or-url))
            (user-error "No command found for opening a file-or-url"))))
      (_ (user-error "Unsupported system-type: %s" system-type)))))

(provide 'akirak-open)
;;; akirak-open.el ends here
