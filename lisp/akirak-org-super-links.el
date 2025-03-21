;;; akirak-org-super-links.el ---  -*- lexical-binding: t -*-

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


;;;###autoload
(defun akirak-org-super-links-info ()
  "Return information on backlinks in the entry."
  (let ((entry-end (org-entry-end-position))
        (count 0)
        latest)
    (catch 'finish
      (save-excursion
        (while (re-search-forward org-drawer-regexp entry-end t)
          (when (string-equal-ignore-case (match-string 1) "BACKLINKS")
            (let ((start (match-end 0))
                  (end (catch 'end
                         (save-excursion
                           (while (re-search-forward org-drawer-regexp entry-end t)
                             (string-equal-ignore-case (match-string 1)
                                                       "END")
                             (throw 'end (match-beginning 0)))))))
              (goto-char start)
              (save-excursion
                (while (re-search-forward org-link-bracket-re end t)
                  (cl-incf count)))
              (while (re-search-forward org-ts-regexp-inactive end t)
                (let ((ts (org-timestamp-from-string (match-string 0))))
                  (unless (and latest
                               (time-less-p (org-timestamp-to-time ts)
                                            (org-timestamp-to-time latest)))
                    (setq latest ts)))))
            (throw 'finish t)))))
    (list :latest latest :count count)))

(provide 'akirak-org-super-links)
;;; akirak-org-super-links.el ends here
