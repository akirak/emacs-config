;;; akirak-passage.el --- A custom wrapper for Passage -*- lexical-binding: t -*-

(require 'transient)

(defconst akirak-passage-buffer "*Passage*")

(defcustom akirak-passage-dir
  (or (getenv "PASSAGE_DIR")
      "~/.passage/store")
  "Pass to the passage password store."
  :type 'directory)

(defcustom akirak-passage-executable "passage"
  "Executable name of passage."
  :type 'file)

(defcustom akirak-passage-selection 'CLIPBOARD
  "Selection to which strings are copied."
  :type 'symbol)

;;;; Utilities

(defun akirak-passage--copy-string (string)
  (gui-set-selection akirak-passage-selection string)
  (message "Copied the password to the clipboard (%d characters). Clearing in 15 seconds"
           (length string))
  (run-with-timer 15 nil (lambda ()
                           (gui-set-selection akirak-passage-selection "")
                           (message "Cleared the clipboard"))))

;;;; Internal API

(defun akirak-passage--account-list (&optional dir)
  (when (file-directory-p akirak-passage-dir)
    (let* ((root (expand-file-name akirak-passage-dir))
           (dir (if dir
                    (expand-file-name dir root)
                  root)))
      (cl-flet
          ((to-ent (file)
             (file-name-sans-extension
              (file-relative-name file root))))
        (thread-last
          (directory-files-recursively dir "\\.age\\'")
          (mapcar #'to-ent)
          (seq-uniq))))))

(defun akirak-passage--run-process (type &rest args)
  (declare (indent 1))
  (when (and (get-buffer-process akirak-passage-buffer)
             (process-live-p (get-buffer-process akirak-passage-buffer)))
    (user-error "Another passage process is running"))
  (when (get-buffer akirak-passage-buffer)
    (with-current-buffer akirak-passage-buffer
      (erase-buffer)))
  (cl-flet
      ((sentinel (process event)
         (cond
          ((string= event "finished\n"))
          ((string= event "open\n"))
          ((and (string= event "exited abnormally with code 1\n")
                (eq type 'edit)))
          (t
           (error "passage %s aborted: %s" args event))))
       ;; Touch is not supported at present. If you are using
       ;; age-plugin-yubikey, you must set the touch policy to never.
       (filter-fn (process string)
         (pcase string
           ((rx (and bol "Please insert "))
            (let ((inp (read-string string)))
              (process-send-string process (concat inp "\n"))))
           ((rx "Enter PIN for ")
            (let ((inp (password-read string string)))
              (process-send-string process (concat inp "\n"))))
           ("Waiting for age-plugin-yubikey...\n")
           ("Password unchanged.\n"
            (message (string-trim-right string "\n")))
           (_
            (with-current-buffer (process-buffer process)
              (insert string))))))
    (let ((process (make-process :name "passage"
                                 :buffer (get-buffer-create akirak-passage-buffer)
                                 :command (cons akirak-passage-executable args)
                                 :connection-type 'pty
                                 :noquery t
                                 :sentinel #'sentinel
                                 :filter #'filter-fn)))
      (pcase-exhaustive type
        (`read
         (while (process-live-p process)
           (accept-process-output process)
           (sit-for 0.2))
         (if (zerop (process-exit-status process))
             (prog1 (with-current-buffer akirak-passage-buffer
                      (goto-char (point-min))
                      (while (and (looking-at (rx eol))
                                  (< (point) (point-max)))
                        (forward-line))
                      (buffer-substring (point) (point-max)))
               (kill-buffer akirak-passage-buffer))
           (error "Non-zero exit code from passage. See %s" akirak-passage-buffer)))
        (`edit)))))

;;;; Infixes

(defvar akirak-passage-current-account nil)

(defclass akirak-passage-account-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-passage-account-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj akirak-passage-account-variable))
  (let ((default (oref obj value)))
    (completing-read (format-prompt "Account" default)
                     (akirak-passage--account-list)
                     nil nil nil nil
                     default)))

(cl-defmethod transient-infix-set ((obj akirak-passage-account-variable) value)
  (oset obj value value)
  (set (oref obj variable) (string-trim-right value "/")))

(cl-defmethod transient-format-value ((obj akirak-passage-account-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (if value
         (propertize value 'face 'transient-value)
       "not set")
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix akirak-passage-set-account ()
  :class 'akirak-passage-account-variable
  :description "Account"
  :variable 'akirak-passage-current-account)

;;;; Prefix

;;;###autoload (autoload 'akirak-passage "akirak-passage" nil 'interactive)
(transient-define-prefix akirak-passage ()
  "Work with the age-based password store interactively."
  [("=" akirak-passage-set-account)]
  ["Read the entry"
   ("w" "Copy password" akirak-passage-copy-password)]
  ["Update the entry"
   ("e" "Edit" akirak-passage-edit-entry)]
  (interactive)
  (unless akirak-passage-current-account
    (setq akirak-passage-current-account (transient-infix-read 'akirak-passage-set-account)))
  (transient-setup 'akirak-passage))

;;;; Suffixes

(defun akirak-passage-copy-password ()
  "Copy the first line of the current password entry."
  (interactive)
  (let ((output (akirak-passage--run-process 'read
                  "show" akirak-passage-current-account)))
    (akirak-passage--copy-string (car (split-string output "\n")))))

(defun akirak-passage-edit-entry ()
  "Edit the current password entry."
  (interactive)
  (with-editor
    (akirak-passage--run-process 'edit
      "edit" akirak-passage-current-account)))

(provide 'akirak-passage)
;;; akirak-passage.el ends here
