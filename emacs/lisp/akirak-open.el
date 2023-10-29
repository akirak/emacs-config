;;; akirak-open.el ---  -*- lexical-binding: t -*-

(defcustom akirak-open-wsl-p nil
  "Non-ni if Emacs is running on Windows Subsystem for Linux."
  :type 'boolean)

;;;###autoload
(defun akirak-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "f")
  (when (file-remote-p file)
    (user-error "Remote file is not supported"))
  (with-current-buffer (generate-new-buffer "*embark open*")
    (let ((file (convert-standard-filename (expand-file-name file))))
      (message "Opening %s externally" file)
      (pcase system-type
        (`darwin
         (call-process "open" nil t nil file))
        (`gnu/linux
         (ensure-list
          (or (when akirak-open-wsl-p
                (when-let (exe (executable-find "wsl-open"))
                  (call-process exe nil t nil file)))
              (when-let (exe (executable-find "handlr"))
                (call-process exe nil t nil "open" file))
              (when-let (exe (executable-find "xdg-open"))
                (call-process exe nil t nil file))
              (user-error "No command found for opening a file"))))
        (_ (user-error "Unsupported system-type: %s" system-type))))))

(provide 'akirak-open)
;;; akirak-open.el ends here
