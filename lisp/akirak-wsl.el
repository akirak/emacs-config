;;; akirak-wsl.el --- Support for Windows Subsystem for Linux -*- lexical-binding: t -*-

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


(defvar akirak-wsl-p nil)

;;;###autoload
(defun akirak-wsl-p ()
  "Return non-nil if the session is running inside Windows Subsystem for Linux."
  (pcase-exhaustive akirak-wsl-p
    (`t t)
    (:no nil)
    (`nil (with-temp-buffer
            (when (ignore-errors (call-process "uname" nil (list t nil) nil "-a"))
              (goto-char (point-min))
              (let ((case-fold-search t))
                (if (re-search-forward "microsoft" nil t)
                    (setq akirak-wsl-p t)
                  (setq akirak-wsl-p :no)
                  nil)))))))

(provide 'akirak-wsl)
;;; akirak-wsl.el ends here
