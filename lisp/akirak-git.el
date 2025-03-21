;;; akirak-git.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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
(defun akirak-git-list-remote-pages ()
  (interactive)
  (let ((url (completing-read "Browse git remote: "
                              (completion-table-with-metadata
                               (akirak-git--remote-urls)
                               '((category . url))))))
    (browse-url url)))

(defun akirak-git--remote-urls ()
  (thread-last
    (process-lines "git" "config" "--list" "--local")
    (mapcar (lambda (line)
              (when (string-match (rx bol "remote." (+ (any "-" alnum)) ".url="
                                      (group (+ anything)))
                                  line)
                (match-string 1 line))))
    (delq nil)
    (mapcar #'akirak-git--git-html-url)))

(defun akirak-git--git-html-url (git-url)
  "Convert GIT-URL to an HTML url."
  (thread-last
    (pcase git-url
      ((rx bol "git@" (group "github.com") ":" (group (+ anything)))
       (concat "https://" (match-string 1 git-url) "/"
               (match-string 2 git-url)))
      (_
       git-url))
    (string-remove-suffix ".git")))

(provide 'akirak-git)
;;; akirak-git.el ends here
