;;; akirak-org-secrets.el --- Use Org files to manage secrets -*- lexical-binding: t -*-

(require 'org)

(defcustom akirak-org-secrets-file nil
  "Org file that tracks secrets in crypted entries."
  :type 'file)

;;;###autoload
(defun akirak-org-secrets-insert ()
  "Insert a secret into the current buffer."
  (interactive)
  (insert (akirak-org-secrets--decrypted-body)))

;;;###autoload
(defun akirak-org-secrets-copy ()
  "Copy a secret to the system clipboard."
  (interactive)
  (let ((secret (akirak-org-secrets--decrypted-body))
        (expire 45))
    (gui-select-text secret)
    (run-with-timer expire nil
                    (lambda () (gui-select-text "")))
    (message "Clear the clipboard in %d sec." expire)))

;;;###autoload
(defun akirak-org-secrets-show ()
  "Display a secret in a buffer."
  (interactive)
  (let ((secret (akirak-org-secrets--decrypted-body))
        (buffer (generate-new-buffer "*secret*"))
        (expire 10))
    (with-current-buffer buffer
      (insert secret))
    (pop-to-buffer buffer)
    (run-with-timer expire nil
                    `(lambda ()
                       (when-let (buffer (get-buffer ,(buffer-name buffer)))
                         (if-let (window (get-buffer-window buffer))
                             (quit-window 'kill window)
                           (kill-buffer buffer)))))
    (message "Kill the buffer in %d sec." expire)))

;;;###autoload
(defun akirak-org-secrets-capture ()
  "Create a new encrypted entry."
  (interactive)
  (let* ((file akirak-org-secrets-file)
         (width (window-width))
         (parent (with-current-buffer (or (find-buffer-visiting file)
                                          (find-file-noselect file))
                   (completing-read "Parent entry: "
                                    (org-map-entries
                                     (lambda ()
                                       (org-format-outline-path (org-get-outline-path t)
                                                                width nil "/"))
                                     "-crypt"))))
         (org-capture-entry `("" ""
                              entry
                              (file+function
                               ,file
                               (lambda ()
                                 (goto-char (point-min))
                                 (akirak-org-goto-or-create-olp
                                  ',(akirak-org-secrets--parse-olp parent))))
                              "* %? :crypt:")))
    (org-capture)))

(defun akirak-org-secrets--parse-olp (string)
  (thread-last
    (split-string string "/")
    (cl-remove-if #'string-empty-p)))

(defun akirak-org-secrets--decrypted-body ()
  "Return the decrypted body of an entry."
  (let ((file akirak-org-secrets-file)
        (width (window-width)))
    (with-current-buffer (or (find-buffer-visiting file)
                             (find-file-noselect file))
      (org-with-wide-buffer
       (let ((olp-string
              (completing-read "Crypted entries: "
                               (org-map-entries
                                (lambda ()
                                  (org-format-outline-path (org-get-outline-path t)
                                                           width nil "/"))
                                "crypt"))))
         (goto-char (org-find-olp (thread-last
                                    (split-string olp-string "/")
                                    (cl-remove-if #'string-empty-p))
                                  t))
         (org-decrypt-entry)
         (org-end-of-meta-data t)
         (buffer-substring-no-properties (point) (org-entry-end-position)))))))

(provide 'akirak-org-secrets)
;;; akirak-org-secrets.el ends here
