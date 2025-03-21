;;; akirak-open.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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


(defvar akirak-open-default-command nil)

;;;###autoload
(defun akirak-open-file-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "f")
  (when (file-remote-p file)
    (user-error "Remote file is not supported"))
  (let ((file (convert-standard-filename (expand-file-name file))))
    (message "Opening %s externally" file)
    (akirak-open-default file)))

;;;###autoload
(defun akirak-open-default (file-or-url &optional _)
  "Open FILE-OR-URL with the default application."
  (with-current-buffer (generate-new-buffer "*embark open*")
    (let ((file-or-url (if (string-match-p (rx bol (>= 2 alnum) ":") file-or-url)
                           file-or-url
                         (convert-standard-filename (expand-file-name file-or-url)))))
      (insert "Opening " file-or-url "\n")
      (pcase (if (and akirak-open-default-command
                      (or (file-exists-p (car akirak-open-default-command))
                          (executable-find (car akirak-open-default-command))))
                 akirak-open-default-command
               (akirak-open-default-command))
        (`(,program . ,options)
         (apply #'call-process program nil t nil (append options (list file-or-url))))))))

(defun akirak-open-default-command ()
  "Return the default program with args for opening a file.

This function also sets `akirak-open-default-command' variable to
the returned value to memorize the result."
  (setq akirak-open-default-command
        (ensure-list (pcase-exhaustive system-type
                       (`darwin "open")
                       (`gnu/linux
                        (or (when (akirak-wsl-p)
                              (or (executable-find "wslview")
                                  (executable-find "wsl-open")))
                            (when-let* ((exe (executable-find "handlr")))
                              (list exe "open"))
                            (executable-find "xdg-open")
                            (error "No program for running the default handler")))))))

;;;###autoload (autoload 'akirak-open-can-use-default-program "akirak-open")
(defalias 'akirak-open-can-use-default-program #'akirak-open-default-command)

(provide 'akirak-open)
;;; akirak-open.el ends here
