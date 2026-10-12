;;; akirak-ssh.el --- SSH integration -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Assisted-by: Devin:SWE-2-High

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

(require 'subr-x)
(require 'akirak-transient)

(defconst akirak-ssh-keygen-buffer "*ssh-keygen*")

(defcustom akirak-ssh-keygen-executable "ssh-keygen"
  "The ssh-keygen executable."
  :type 'file)

;;;; Key type

(defvar akirak-ssh-keygen-type "ed25519")

(transient-define-infix akirak-ssh-keygen-set-type ()
  :class 'akirak-transient-choice-variable
  :variable 'akirak-ssh-keygen-type
  :choices '("ed25519" "ed25519-sk" "ecdsa" "ecdsa-sk"
             "mldsa44-ed25519" "rsa")
  :prompt "Key type: "
  :description "Key type (-t)")

;;;; Passphrase

(defvar akirak-ssh-keygen-use-passphrase nil)

(transient-define-infix akirak-ssh-keygen-toggle-passphrase ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-ssh-keygen-use-passphrase
  :description "Set a passphrase (-N)")

;;;; FIDO options

(defvar akirak-ssh-keygen-verify-required nil)

(transient-define-infix akirak-ssh-keygen-toggle-verify-required ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-ssh-keygen-verify-required
  :description "Require verification (-O verify-required)")

(defvar akirak-ssh-keygen-application nil)

(transient-define-infix akirak-ssh-keygen-set-application ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-ssh-keygen-application
  :nullify t
  :prompt "Application: "
  :description "Application (-O application=)")

;;;; Prefix

;;;###autoload (autoload 'akirak-ssh-keygen "akirak-ssh" nil 'interactive)
(transient-define-prefix akirak-ssh-keygen ()
  "Generate an SSH key pair in a temporary directory."
  ["Options"
   ("-t" akirak-ssh-keygen-set-type)
   ("-N" akirak-ssh-keygen-toggle-passphrase)
   ("-v" akirak-ssh-keygen-toggle-verify-required)
   ("-a" akirak-ssh-keygen-set-application)]
  ["Generate"
   ("i" "Insert the private key into the buffer and save the public key to kill ring"
    akirak-ssh-keygen--insert)]
  (interactive)
  (transient-setup 'akirak-ssh-keygen))

;;;; Suffixes

(defun akirak-ssh-keygen--insert ()
  (interactive)
  (pcase-let ((`(,private ,public) (akirak-ssh-keygen--generate)))
    (insert private)
    (kill-new public)
    (message "Saved the public key to the kill ring")))

(defun akirak-ssh-keygen--generate ()
  "Generate a key pair and return a list (PRIVATE-KEY PUBLIC-KEY).

The ssh-keygen process is run on a PTY so that it can interact
with the user, e.g. for entering passphrases, FIDO2 PINs, and
touching the authenticator."
  (let* ((dir (make-temp-file "akirak-ssh-keygen-" t))
         (base (expand-file-name (concat "id_" akirak-ssh-keygen-type) dir))
         (buffer (get-buffer-create akirak-ssh-keygen-buffer)))
    (with-current-buffer buffer
      (erase-buffer))
    (unwind-protect
        (let ((process
               (make-process
                :name "ssh-keygen"
                :buffer buffer
                :command (append
                          (list akirak-ssh-keygen-executable
                                "-q"
                                "-t" akirak-ssh-keygen-type
                                "-f" base)
                          (unless akirak-ssh-keygen-use-passphrase
                            (list "-N" ""))
                          (when akirak-ssh-keygen-verify-required
                            (list "-O" "verify-required"))
                          (when akirak-ssh-keygen-application
                            (list "-O" (concat "application="
                                               akirak-ssh-keygen-application))))
                :connection-type 'pty
                :noquery t
                :sentinel #'ignore
                :filter #'akirak-ssh-keygen--filter)))
          (condition-case nil
              (while (process-live-p process)
                (accept-process-output process)
                (sit-for 0.2))
            (quit
             (kill-process process)
             (user-error "Aborted by the user")))
          (if (zerop (process-exit-status process))
              (prog1 (list (akirak-ssh-keygen--file-contents base)
                           (akirak-ssh-keygen--file-contents (concat base ".pub")))
                (kill-buffer buffer))
            (error "ssh-keygen failed. See %s" akirak-ssh-keygen-buffer)))
      (delete-directory dir t))))

(defun akirak-ssh-keygen--filter (process string)
  "Handle output STRING from the ssh-keygen PROCESS.

Respond to interactive prompts and echo status messages, such as
requests to touch the authenticator, to the echo area."
  (pcase string
    ((rx (or "Enter passphrase"
             "Enter same passphrase again"
             "Enter PIN"))
     (let ((input (condition-case nil
                      (read-passwd (string-trim string))
                    (quit
                     (interrupt-process process)
                     (user-error "Aborted by the user")))))
       (process-send-string process (concat input "\n"))))
    ((rx "Overwrite (y/n)")
     (process-send-string process
                          (if (yes-or-no-p (concat (string-trim string) " "))
                              "y\n"
                            "n\n")))
    (_
     (when (string-match-p "touch your authenticator" string)
       (message "%s" (string-trim string)))))
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (point-max))
        (insert string)))))

(defun akirak-ssh-keygen--file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(provide 'akirak-ssh)
;;; akirak-ssh.el ends here
