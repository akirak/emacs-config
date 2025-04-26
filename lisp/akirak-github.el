;;; akirak-github.el --- GitHub integration features -*- lexical-binding: t -*-

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

;; This library defines the following commands:
;;
;; - `akirak-github-browse-issues-web`: Opens the issues page of the current
;;   GitHub repository in your web browser. You can optionally provide a search
;;   query to filter the issues.
;;
;; - `akirak-github-browse-prs-web`: Opens the pull requests page of the current
;;   GitHub repository in your web browser. You can optionally provide a search
;;   query to filter the pull requests.
;;
;; Both commands use the GitHub CLI (`gh`) to open the respective pages in the
;; browser. They are mostly intended to navigate issues on repositories
;; maintained by other people and organizations.

;;; Code:

(defcustom akirak-github-gh-executable "gh"
  ""
  :type 'file)

;;;###autoload
(defun akirak-github-browse-issues-web (&optional query)
  "Browse issues of the current GitHub repository online."
  (interactive
   (list (read-string "Query for GitHub issues (optional): ")))
  (apply #'start-process "gh" nil akirak-github-gh-executable
         "issue" "list" "--web"
         (when (and query
                    (not (string-empty-p query)))
           (list "--search" query))))

;;;###autoload
(defun akirak-github-browse-prs-web (&optional query)
  "Browse PRs of the current GitHub repository online."
  (interactive
   (list (read-string "Query for GitHub PRs (optional): ")))
  (apply #'start-process "gh" nil akirak-github-gh-executable
         "pr" "list" "--web"
         (when (and query
                    (not (string-empty-p query)))
           (list "--search" query))))

(provide 'akirak-github)
;;; akirak-github.el ends here
