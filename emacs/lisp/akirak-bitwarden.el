;;; akirak-bitwarden.el --- Bitwarden (rbw) wrapper -*- lexical-binding: t -*-

(defconst akirak-bitwarden-buffer "*Bitwarden rbw*"
  "Name of the buffer used to display a password.")

(defcustom akirak-bitwarden-rbw-executable "rbw"
  "Name of rbw, an unofficial Bitwarden CLI."
  :type 'file)

(defvar-keymap akirak-bitwarden-rbw-mode-map
  "C-c C-c" #'with-editor-finish
  "C-c C-k" #'with-editor-cancel)

(define-derived-mode akirak-bitwarden-rbw-mode fundamental-mode
  "rbw")

(defun akirak-bitwarden-complete-entry (prompt &optional require-match)
  (completing-read prompt
                   (process-lines akirak-bitwarden-rbw-executable "list")
                   nil require-match))

;;;###autoload
(defun akirak-bitwarden-show (entry)
  "Display a password ENTRY."
  (interactive (list (akirak-bitwarden-complete-entry "Password entry: "
                                                      'require-match)))
  (with-current-buffer (get-buffer-create akirak-bitwarden-buffer)
    (erase-buffer)
    (unless (zerop (call-process akirak-bitwarden-rbw-executable nil (list t nil) nil
                                 "get" "--full" entry))
      (error "rbw returned non-zero"))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun akirak-bitwarden-copy (entry)
  "Copy the password of an ENTRY."
  (interactive (list (akirak-bitwarden-complete-entry "Password entry: "
                                                      'require-match)))
  (with-temp-buffer
    (unless (zerop (call-process akirak-bitwarden-rbw-executable nil (list t nil) nil
                                 "get" entry))
      (error "rbw returned non-zero"))
    (gui-set-selection 'CLIPBOARD (buffer-string))
    (message "Copied the password to the clipboard (%d characters). Clearing in 15 seconds"
             (buffer-size))
    (run-with-timer 15 nil (lambda ()
                             (gui-set-selection 'CLIPBOARD "")
                             (message "Cleared the clipboard")))))

;;;###autoload
(defun akirak-bitwarden-edit (entry)
  "Edit a password ENTRY."
  (interactive (list (akirak-bitwarden-complete-entry "Edit a password entry: ")))
  (with-editor-async-shell-command
   (mapconcat #'shell-quote-argument
              (list akirak-bitwarden-rbw-executable "edit" entry)
              " ")))

;;;###autoload
(defun akirak-bitwarden-add (entry)
  "Add a new password ENTRY."
  (interactive (list "sNew entry: "))
  (with-editor-async-shell-command
   (mapconcat #'shell-quote-argument
              (list akirak-bitwarden-rbw-executable "add" entry)
              " ")))

;;;###autoload
(defun akirak-bitwarden-remove (entry)
  "Remove a password ENTRY."
  (interactive (list (akirak-bitwarden-complete-entry "Remove a password entry: ")))
  (when (yes-or-no-p (format-message "Are you sure you want to delete \"%s\"? " entry))
    (unless (zerop (call-process akirak-bitwarden-rbw-executable nil nil nil
                                 "remove" entry))
      (error "rbw returned non-zero"))))

(provide 'akirak-bitwarden)
;;; akirak-bitwarden.el ends here
