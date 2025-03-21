;;; akirak-japanese.el --- Extra support for the Japanese language -*- lexical-binding: t -*-

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


(declare-function engine/search-google "akirak-engine-mode")

(defvar akirak-japanese-search-history nil)

;;;###autoload
(defun akirak-japanese-search ()
  "Search a Japanese query

This command is deprecated. Use `akirak-scratch-japanese'
instead."
  (interactive)
  (let ((query (minibuffer-with-setup-hook
                   #'riben-mode
                 (completing-read "Search: "
                                  #'akirak-japanese--search-completions
                                  nil nil
                                  (when (use-region-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end)))
                                  'akirak-japanese-search-history))))
    (engine/search-google query)))

(defun akirak-japanese--search-completions (string pred action)
  "Return a completion table for URLS."
  (if (eq action 'metadata)
      '(metadata . ((category . japanese-query)))
    (complete-with-action action
                          akirak-japanese-search-history
                          string pred)))

(provide 'akirak-japanese)
;;; akirak-japanese.el ends here
