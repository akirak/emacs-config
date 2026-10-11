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

(require 'akirak-transient)

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

;;;; Prefix

;;;###autoload (autoload 'akirak-ssh-keygen "akirak-ssh" nil 'interactive)
(transient-define-prefix akirak-ssh-keygen ()
  "Generate an SSH key pair in a temporary directory."
  ["Options"
   ("-t" akirak-ssh-keygen-set-type)
   ("-N" akirak-ssh-keygen-toggle-passphrase)]
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
  "Generate a key pair and return a list (PRIVATE-KEY PUBLIC-KEY)."
  (let* ((dir (make-temp-file "akirak-ssh-keygen-" t))
         (base (expand-file-name (concat "id_" akirak-ssh-keygen-type) dir))
         (passphrase (if akirak-ssh-keygen-use-passphrase
                         (read-passwd "Passphrase: " t)
                       "")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (unless (zerop (call-process akirak-ssh-keygen-executable
                                         nil (list t t) nil
                                         "-q" "-t" akirak-ssh-keygen-type
                                         "-N" passphrase
                                         "-f" base))
              (error "ssh-keygen failed: %s" (string-trim (buffer-string)))))
          (list (akirak-ssh-keygen--file-contents base)
                (akirak-ssh-keygen--file-contents (concat base ".pub"))))
      (delete-directory dir t))))

(defun akirak-ssh-keygen--file-contents (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(provide 'akirak-ssh)
;;; akirak-ssh.el ends here
