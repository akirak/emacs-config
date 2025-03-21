;;; akirak-shr.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Akira Komamura

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


(defvar-keymap akirak-shr-mode-map
  :doc "Keymap for akirak-shr mode."
  "C-c C-p" #'akirak-shr-previous-heading
  "C-c C-n" #'akirak-shr-next-heading)

;;;###autoload
(define-minor-mode akirak-shr-mode
  "Minor mode to provide extra features for shr-based modes.")

;;;###autoload
(defun akirak-shr-next-heading ()
  (interactive)
  (end-of-line)
  (if (text-property-search-forward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (beginning-of-line)
    (user-error "No more heading below")))

;;;###autoload
(defun akirak-shr-previous-heading ()
  (interactive)
  (beginning-of-line)
  (if (text-property-search-backward
       'face '(shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6) #'akirak-shr--predicate
       'not-current)
      (beginning-of-line)
    (user-error "No more heading above")))

(defun akirak-shr--predicate (values value)
  (seq-intersection values (ensure-list value)))

(provide 'akirak-shr)
;;; akirak-shr.el ends here
