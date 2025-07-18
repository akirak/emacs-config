;;; akirak-passage.el --- A custom wrapper for Passage -*- lexical-binding: t -*-

;; Copyright (C) 2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'transient)
(require 'with-editor)
(require 'auth-source)

(defconst akirak-passage-buffer "*Passage*")

(defcustom akirak-passage-executable "passage"
  "Executable name of passage."
  :type 'file)

(defcustom akirak-passage-selection 'CLIPBOARD
  "Selection to which strings are copied."
  :type 'symbol)

(defvar akirak-passage-dir nil)
(defvar akirak-passage-process-environment nil)

;;;; Utilities

(defun akirak-passage--copy-string (string)
  (gui-set-selection akirak-passage-selection string)
  (message "Copied the password to the clipboard (%d characters). Clearing in 15 seconds"
           (length string))
  (run-with-timer 15 nil (lambda ()
                           (gui-set-selection akirak-passage-selection "")
                           (message "Cleared the clipboard"))))

(defun akirak-passage-configure-from-shell ()
  (require 'exec-path-from-shell)
  (let* ((default-directory "~/")
         (alist (exec-path-from-shell-getenvs '("PASSAGE_DIR"
                                                "PASSAGE_AGE"
                                                "PASSAGE_IDENTITIES_FILE"
                                                "PASSAGE_RECIPIENTS_FILE"))))
    (setq akirak-passage-process-environment
          (thread-last
            alist
            (seq-filter #'cdr)
            (mapcar (pcase-lambda (`(,key . ,value))
                      (concat key "=" value)))))
    (setq akirak-passage-dir (cdr (assoc "PASSAGE_DIR" alist)))))

(defun akirak-passage--git-commit (message)
  (with-current-buffer (get-buffer-create akirak-passage-buffer)
    (let ((default-directory akirak-passage-dir)
          (process-environment (append akirak-passage-process-environment
                                       process-environment)))
      (unless (zerop (call-process "git" nil t nil
                                   "add" "."))
        (user-error "git-add failed"))
      (unless (zerop (call-process "git" nil t nil
                                   "commit" "-a" "-m" message))
        (user-error "git-commit failed")))))

;;;; Internal API

(defun akirak-passage--account-list (&optional dir)
  (if (file-directory-p akirak-passage-dir)
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
            (seq-uniq))))
    (user-error "akirak-passage-dir is not set")))

(defun akirak-passage--account-exists-p (account)
  (if (file-directory-p akirak-passage-dir)
      (file-readable-p (expand-file-name (concat account ".age") akirak-passage-dir))
    (user-error "akirak-passage-dir is not set")))

(defun akirak-passage--get-password (account)
  "Return the first line of the password entry of ACCOUNT."
  (thread-first
    (akirak-passage--run-process nil "show" account)
    (split-string "\n")
    (car)))

(defun akirak-passage--run-process (edit-hook &rest args)
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
          ((string= event "finished\n")
           (when (functionp edit-hook)
             (funcall edit-hook)))
          ((string= event "open\n"))
          ((and (string= event "exited abnormally with code 1\n")
                edit-hook))
          (t
           (user-error "passage %s aborted: %s" args event))))
       ;; Touch is not supported at present. If you are using
       ;; age-plugin-yubikey, you must set the touch policy to never.
       (filter-fn (process string)
         (pcase string
           ((rx (and bol "Please insert "))
            (let ((inp (condition-case nil
                           (read-string string)
                         (quit
                          (interrupt-process process)
                          (user-error "Aborted by the user")))))
              (process-send-string process (concat inp "\n"))))
           ((rx "Enter PIN for ")
            (let ((inp (condition-case nil
                           (password-read string string)
                         (quit
                          (interrupt-process process)
                          (user-error "Aborted by the user")))))
              (process-send-string process (concat inp "\n"))))
           ("Waiting for age-plugin-yubikey...\n")
           ("Password unchanged.\n"
            (message (string-trim-right string "\n")))
           (_
            (with-current-buffer (process-buffer process)
              (unless (string-empty-p (string-trim-right string))
                ;; Clear the previous responses as it can contain "y".
                (replace-region-contents (point-min)
                                         (point)
                                         string)))))))
    (let* ((process-environment (append akirak-passage-process-environment
                                        process-environment))
           (process (make-process :name "passage"
                                  :buffer (get-buffer-create akirak-passage-buffer)
                                  :command (cons akirak-passage-executable args)
                                  :connection-type 'pty
                                  :noquery t
                                  :sentinel #'sentinel
                                  :filter #'filter-fn)))
      (condition-case nil
          (unless edit-hook
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
              (user-error "Non-zero exit code from passage. See %s" akirak-passage-buffer)))
        (quit
         ;; Clean up
         (kill-process process)
         (user-error "Abnormally exited"))))))

;;;; Infixes

(defvar akirak-passage-current-account nil)

(defclass akirak-passage-account-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-passage-account-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj akirak-passage-account-variable))
  (akirak-passage--read-account (oref obj value)))

(defun akirak-passage--read-account (default)
  (completing-read (format-prompt "Account" default)
                   (akirak-passage--account-list)
                   nil nil nil nil
                   default))

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
   :class transient-row
   ("e" "Edit" akirak-passage-edit-entry)
   ("m" "Rename" akirak-passage-rename-entry)
   ("D" "Delete" akirak-passage-delete-entry)]
  (interactive)
  (unless akirak-passage-current-account
    (setq akirak-passage-current-account (akirak-passage--read-account nil)))
  (transient-setup 'akirak-passage))

;;;; Suffixes

(defun akirak-passage-copy-password ()
  "Copy the first line of the current password entry."
  (interactive)
  (akirak-passage--copy-string (akirak-passage--get-password akirak-passage-current-account)))

(defun akirak-passage-edit-entry ()
  "Edit the current password entry."
  (interactive)
  (with-editor
    (akirak-passage--run-process
     (lambda ()
       (akirak-passage--git-commit (format "Edited %s" akirak-passage-current-account)))
     "edit" akirak-passage-current-account)))

(defun akirak-passage-rename-entry ()
  "Rename or move the current entry."
  (interactive)
  (let ((new-account (read-string "Rename the entry: " akirak-passage-current-account)))
    (akirak-passage--run-process nil
      "mv" "--force" akirak-passage-current-account new-account)
    (akirak-passage--git-commit (format "Renamed %s to %s"
                                        akirak-passage-current-account
                                        new-account))
    (message "Renamed the password entry")
    (setq akirak-passage-current-account new-account)))

(defun akirak-passage-delete-entry ()
  "Delete the current entry."
  (interactive)
  (when (yes-or-no-p (format "Delete the password entry \"%s\"? "
                             akirak-passage-current-account))
    (akirak-passage--run-process nil
      "rm" "--force" akirak-passage-current-account)
    (akirak-passage--git-commit (format "Deleted %s" akirak-passage-current-account))
    (message "Deleted the password entry")
    (setq akirak-passage-current-account nil)))

(defun akirak-passage-add-process-environment (env account)
  (cons (concat env "=" (akirak-passage--get-password account))
        process-environment))

;;;; Auth-source support

;; Based on the implementation of auth-source-pass.el by Damien Cassou et al.

;; To enable this feature, add @='passage to @='auth-sources and run the
;; following code:

;; (if (boundp 'auth-source-backend-parser-functions)
;;     (add-hook 'auth-source-backend-parser-functions #'akirak-passage-auth-source-parse)
;;   (advice-add 'auth-source-backend-parse :before-until #'akirak-passage-auth-source-parse))

;; This needs to be defined before `akirak-passage-auth-source-backend' variable
;; because eieio checks the type of its arguments.
(cl-defun akirak-passage-auth-source-search (&rest spec
                                                   &key backend type host
                                                   user port
                                                   require max
                                                   &allow-other-keys)
  (cl-assert (or (null type) (eq type (oref backend type))) t
             "Invalid passage %s %s")
  (cond
   ((eq host t)
    (warn "Not supporting host wildcards")
    nil)
   ((null host)
    nil)
   (t
    (let ((account (format "%s/%s"
                           (if port
                               (format "%d:%d" host port)
                             host)
                           user)))
      (when (akirak-passage--account-exists-p account)
        (list (list :secret (akirak-passage--get-password account))))))))

(defvar akirak-passage-auth-source-backend
  (make-instance 'auth-source-backend
                 :source "."
                 :type 'passage
                 :search-function #'akirak-passage-auth-source-search))

;;;###autoload
(defun akirak-passage-auth-source-parse (entry)
  (when (eq entry 'passage)
    (auth-source-backend-parse-parameters entry akirak-passage-auth-source-backend)))

(provide 'akirak-passage)
;;; akirak-passage.el ends here
