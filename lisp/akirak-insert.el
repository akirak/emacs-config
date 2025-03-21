;;; akirak-insert.el --- Skeletons -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

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


(require 'skeleton)

;;;###autoload
(defun akirak-insert ()
  "Complete an insertion command."
  (interactive)
  (let ((command (read-extended-command-1 "Insert: " "akirak-insert-")))
    (call-interactively (intern command))))

(defun akirak-insert--filename ()
  (if (eq major-mode 'nov-mode)
      nov-file-name
    (buffer-file-name (buffer-base-buffer))))

(defun akirak-insert--file-name-maybe-minibuf ()
  (if (minibufferp)
      (with-minibuffer-selected-window
        (akirak-insert--filename))
    (akirak-insert--filename)))

;;;###autoload (autoload 'akirak-insert-basename "akirak-insert")
(define-skeleton akirak-insert-basename
  "Insert the base name of the buffer." nil
  (file-name-base (akirak-insert--file-name-maybe-minibuf)))

;;;###autoload (autoload 'akirak-insert-basename-pascalcased "akirak-insert")
(define-skeleton akirak-insert-basename-pascalcased
  "Insert the base name of the buffer, pascal-cased." nil
  (progn
    (require 'string-inflection)
    (thread-last
      (file-name-base (akirak-insert--file-name-maybe-minibuf))
      (string-inflection-upper-camelcase-function))))

;;;###autoload (autoload 'akirak-insert-filename-from-project "akirak-insert")
(define-skeleton akirak-insert-filename-from-project
  "Insert the file name of the buffer, relative from the project root." nil
  (let* ((filename (akirak-insert--file-name-maybe-minibuf))
         (root (expand-file-name (project-root (project-current
                                                nil (file-name-directory filename))))))
    (file-relative-name filename root)))

;;;###autoload (autoload 'akirak-insert-directory "akirak-insert")
(define-skeleton akirak-insert-directory
  "Insert the current default directory." nil
  (expand-file-name default-directory))

;;;###autoload (autoload 'akirak-insert-abbreviated-directory "akirak-insert")
(define-skeleton akirak-insert-abbreviated-directory
  "Insert the current default directory, abbreviated." nil
  (abbreviate-file-name default-directory))

;;;###autoload (autoload 'akirak-insert-project-name "akirak-insert")
(define-skeleton akirak-insert-project-name
  "Insert the base name of the buffer." nil
  (thread-last
    (project-current)
    (project-root)
    (string-remove-suffix "/")
    (file-name-nondirectory)))

;;;###autoload (autoload 'akirak-insert-project-name-pascalcased "akirak-insert")
(define-skeleton akirak-insert-project-name-pascalcased
  "Insert the base name of the buffer, pascal-cased." nil
  (progn
    (require 'string-inflection)
    (thread-last
      (project-current)
      (project-root)
      (string-remove-suffix "/")
      (file-name-nondirectory)
      (string-inflection-upper-camelcase-function))))

;;;###autoload (autoload 'akirak-insert-from-lisp "akirak-insert")
(define-skeleton akirak-insert-from-lisp
  "Insert the result of a lisp expression." nil
  (format "%s" (eval (minibuffer-with-setup-hook
                         #'emacs-lisp-mode
                       (read--expression "Eval: ")))))

;;;###autoload (autoload 'akirak-insert-iso8601-date "akirak-insert")
(define-skeleton akirak-insert-iso8601-date
  "Insert date in the ISO-8601 format." nil
  (format-time-string "%F"))

;;;###autoload (autoload 'akirak-insert-iso8601-read-date "akirak-insert")
(define-skeleton akirak-insert-iso8601-read-date
  "Insert date in the ISO-8601 format." nil
  (org-read-date))

;;;###autoload (autoload 'akirak-insert-user-full-name "akirak-insert")
(define-skeleton akirak-insert-user-full-name
  "Insert the full name of the user." nil
  user-full-name)

;;;###autoload (autoload 'akirak-insert-user-mail-address "akirak-insert")
(define-skeleton akirak-insert-user-mail-address
  "Insert the user's email address in the ISO-8601 format." nil
  user-mail-address)

;;;###autoload (autoload 'akirak-insert-org-clock-heading "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-org-clock-heading
  "Insert the heading of the currently clocked entry." nil
  (if (and (require 'org-clock nil t)
           (org-clocking-p))
      (save-current-buffer
        (require 'ol)
        (org-link-display-format (org-entry-get org-clock-marker "ITEM")))
    (user-error "Not clocking in")))

;;;###autoload (autoload 'akirak-insert-window-title "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-window-title
  "Insert a string from the title of a window on the desktop"
  (require 'akirak-window-system)
  (replace-regexp-in-string
   (rx " — Mozilla Firefox" eos)
   ""
   (akirak-window-system-complete-title "Window title: ")))

;;;###autoload (autoload 'akirak-insert-which-function "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-which-function
  "Insert the function name retrieved by `which-function'."
  nil
  (or (if (minibufferp)
          (with-minibuffer-selected-window
            (which-function))
        (which-function))
      (user-error "No name is returned by which-function")))

(provide 'akirak-insert)
;;; akirak-insert.el ends here
