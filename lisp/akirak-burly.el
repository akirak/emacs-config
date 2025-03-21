;;; akirak-burly.el --- Custom commands based on burly -*- lexical-binding: t -*-

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


(require 'burly)

(defvar akirak-burly-clock-data nil)

;;;###autoload
(defun akirak-burly-clock-push ()
  "Save the window configuration for the currently running clock."
  (interactive)
  (push `(,(akirak-burly--clock-id)
          ,org-clock-heading
          ,(burly-windows-url))
        akirak-burly-clock-data))

;;;###autoload
(defun akirak-burly-clock-pop ()
  "Restore the latest window configuration for the clock and delete it."
  (interactive)
  (if-let* ((entry (assoc (akirak-burly--clock-id) akirak-burly-clock-data)))
      (pcase-exhaustive entry
        (`(,_ ,_ ,url)
         (delete entry akirak-burly-clock-data)
         (burly-open-url url)))
    (user-error "Not finding an entry for the ID")))

(defun akirak-burly--clock-id ()
  (if (org-clocking-p)
      (org-id-get org-clock-hd-marker t)
    (user-error "Not clocking in")))

(provide 'akirak-burly)
;;; akirak-burly.el ends here
