;;; akirak-visual-scroll.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

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


(defvar-keymap akirak-visual-scroll-mode-map
  :doc "Keymap for visual scroll mode."
  "C-v"     #'akirak-visual-scroll-page-forward
  "M-v"     #'akirak-visual-scroll-page-backward)

;;;###autoload
(define-minor-mode akirak-visual-scroll-mode
  "Scrolling according to visible area in the window.")

(defun akirak-visual-scroll-page-forward ()
  (interactive)
  (goto-char (1- (window-end)))
  (beginning-of-visual-line)
  (recenter 0 t))

(defun akirak-visual-scroll-page-backward ()
  (interactive)
  (goto-char (window-start))
  (recenter -1 t)
  (goto-char (window-start)))

(provide 'akirak-visual-scroll)
;;; akirak-visual-scroll.el ends here
