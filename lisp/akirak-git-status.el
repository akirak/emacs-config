;;; akirak-git-status.el ---  -*- lexical-binding: t -*-

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


(require 'magit-section)
(require 'taxy)
(require 'akirak-project)

(defun akirak-git-status--maybe-scan-projects ()
  (unless (and akirak-project-last-rescan
               (< (- (float-time)
                     (float-time akirak-project-last-rescan))
                  3600))
    (akirak-project-rescan)))

(defun akirak-git-status-dirty-projects ()
  "List dirty projects."
  (interactive)
  (project-known-project-roots)
  )

(provide 'akirak-git-status)
;;; akirak-git-status.el ends here
