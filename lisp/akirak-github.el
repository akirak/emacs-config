;;; akirak-github.el --- GitHub integration features -*- lexical-binding: t -*-

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


(defcustom akirak-github-global-code-search-url
  "https://github.com/search?type=code&q=%s"
  "URL with a placeholder"
  :type 'string)

;;;###autoload
(defun akirak-github-search-code (query)
  (interactive (list (read-from-minibuffer "GitHub search: "
                                           (format "language:%s %s"
                                                   (akirak-github--language-name)
                                                   (or (thing-at-point 'symbol t)
                                                       "")))))
  (browse-url (format akirak-github-global-code-search-url query)))

(defun akirak-github--language-name (&optional mode)
  "Return the name of the language of the current major MODE."
  ;; To set a proper value of org-src-lang-modes
  (require 'org-src)
  (let ((lang (string-remove-suffix "-mode" (symbol-name (or mode major-mode)))))
    (or (car (rassq (intern lang) org-src-lang-modes))
        (string-remove-suffix "-ts" lang))))

(provide 'akirak-github)
;;; akirak-github.el ends here
