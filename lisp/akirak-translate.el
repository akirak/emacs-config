;;; akirak-translate.el ---  -*- lexical-binding: t -*-

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


(require 'go-translate)

;;;###autoload
(defun akirak-translate-org-heading ()
  "Translate the current Org heading."
  (interactive)
  (gts-translate (gts-translator
                  :picker (akirak-translate-org-heading-picker)
                  :engines (gts-bing-engine)
                  :render (akirak-translate-org-heading-render))))

(defclass akirak-translate-org-heading-picker (gts-picker)
  ())

(cl-defmethod gts-pick ((o akirak-translate-org-heading-picker))
  (let ((text (org-get-heading t t t t)))
    (when (= 0 (length (if text (string-trim text) "")))
      (user-error "The Org heading is empty"))
    (let ((path (gts-path o text)))
      (setq gts-picker-current-path path)
      (cl-values text path))))

(defclass akirak-translate-org-heading-render (gts-render) ())

(cl-defmethod gts-out ((_ akirak-translate-org-heading-render) task)
  (with-slots (result from ecode) task
    (if ecode
        (user-error "%s" result)
      (org-entry-put nil (concat "translation_" from)
                     (org-get-heading t t t t))
      (org-back-to-heading)
      (when (looking-at org-complex-heading-regexp)
        (pcase (seq-drop (match-data) 8)
          (`(,beg ,end . ,_)
           (delete-region beg end)
           (goto-char beg)
           (insert result)))))))

(provide 'akirak-translate)
;;; akirak-translate.el ends here
