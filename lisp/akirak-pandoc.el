;;; akirak-pandoc.el --- Convenient utilities using pandoc -*- lexical-binding: t -*-

(defcustom akirak-pandoc-executable "pandoc"
  ""
  :type 'file)

(defvar akirak-pandoc-input-formats nil)

;;;###autoload
(defun akirak-pandoc-save-as-org (begin end)
  "Convert the region to Org and save the result to kill ring."
  (interactive "r")
  (let ((format (or (akirak-pandoc--buffer-input-format)
                    (completing-read "Input format: "
                                     (akirak-pandoc-input-formats)
                                     nil t)))
        (outbuf (generate-new-buffer "*pandoc*"))
        (errfile (make-temp-file "pandoc-errors-")))
    (unwind-protect
        (progn
          (unless (zerop (call-process-region begin end akirak-pandoc-executable
                                              nil (list outbuf errfile)
                                              (concat "--from=" format)
                                              "--to=org"
                                              "-"))
            (error "pandoc failed: %s"
                   (with-temp-buffer
                     (insert-file-contents errfile)
                     (buffer-string))))
          (kill-new (with-current-buffer outbuf (buffer-string))))
      (delete-file errfile)
      (kill-buffer outbuf))))

(defun akirak-pandoc--buffer-input-format ()
  "Return a pandoc input format for the buffer."
  (if (and (eq major-mode 'markdown-mode)
           (string-match-p "^README\\." (file-name-nondirectory (buffer-file-name))))
      "gfm"
    (car (member (string-remove-suffix "-mode" (symbol-name major-mode))
                 (akirak-pandoc-input-formats)))))

(defun akirak-pandoc-input-formats ()
  (or akirak-pandoc-input-formats
      (setq pandoc-input-formats
            (process-lines akirak-pandoc-executable "--list-input-formats"))))

(provide 'akirak-pandoc)
;;; akirak-pandoc.el ends here
