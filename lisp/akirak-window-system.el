;;; akirak-window-system.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

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


(defcustom akirak-window-system-alist
  '((hyprland
     :read-window akirak-hyprland-read-window
     :require akirak-hyprland))
  "See `akirak-window-system-type' for the types."
  :type '(alist :key-type symbol
                :value-type plist))

(defun akirak-window-system-type ()
  (when (memq window-system '(x pgtk))
    (pcase (getenv "XDG_CURRENT_DESKTOP")
      ("Hyprland"
       'hyprland))))

(defun akirak-window-system--plist ()
  (let* ((type (or (akirak-window-system-type)
                   (user-error "Window system unknown")))
         (plist (cdr (assq type akirak-window-system-alist)))
         (lib (plist-get plist :require)))
    (when lib
      (require lib))
    plist))

;;;###autoload
(defun akirak-window-system-complete-title (prompt)
  "Read the title of a window on the desktop."
  (funcall (plist-get (akirak-window-system--plist)
                      :read-window)
           prompt))

(provide 'akirak-window-system)
;;; akirak-window-system.el ends here
