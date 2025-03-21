;;; akirak-git-commit.el ---  -*- lexical-binding: t -*-

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


(defconst akirak-git-commit-log-drawer-start-re
  (rx bol (* blank) ":GITCOMMITS:" (or blank eol)))

;;;###autoload
(define-minor-mode akirak-git-commit-log-to-org-clock-mode
  "Log commit messages to the clocked Org entry."
  :lighter " LogCommitsToOrg"
  :global t
  (cond
   ((bound-and-true-p akirak-git-commit-log-to-org-clock-mode)
    (add-hook 'git-commit-post-finish-hook
              #'akirak-git-commit-log-to-org-clock))
   (t
    (remove-hook 'git-commit-post-finish-hook
                 #'akirak-git-commit-log-to-org-clock))))

(cl-defun akirak-git-commit-log-to-org-clock-1 (&key message rev dir marker)
  (require 'akirak-org-clock)
  (let* ((default-directory dir)
         (rev-link (when (and (require 'orgit nil t)
                              (orgit-rev-store-1 rev))
                     (org-link-make-string
                      (plist-get org-store-link-plist :link)
                      (truncate-string-to-width rev 7))))
         (case-fold-search t))
    (with-current-buffer (marker-buffer marker)
      (when (and (>= (marker-position marker) (point-min))
                 (<= (marker-position marker) (point-max)))
        (save-excursion
          (goto-char marker)
          (when (equal (org-get-todo-state) "REVIEW")
            (org-todo "UNDERWAY")
            (org-set-tags (cl-remove-duplicates
                           (cons "@failed"
                                 (org-get-tags nil 'local))
                           :test #'equal)))
          (if (re-search-forward akirak-git-commit-log-drawer-start-re
                                 (org-entry-end-position)
                                 t)
              (progn
                (re-search-forward org-drawer-regexp)
                (beginning-of-line))
            ;; It's better for the drawer to not precede a drawer for
            ;; backlinks (if any), so the commit log drawer should be the last
            ;; drawer.
            (org-end-of-meta-data t)
            (when (re-search-backward (rx nonl "\n") nil t)
              (goto-char (match-end 0)))
            (insert ":GITCOMMITS:\n:END:\n")
            (beginning-of-line 0))
          (insert (akirak-git-commit--build-log-line
                    :rev-link rev-link
                    :message message)
                  "\n"))))))

(defun akirak-git-commit-log-to-org-clock ()
  (when (and (featurep 'org-clock)
             (org-clocking-p))
    (let* ((buffer (or (akirak-org-clock--capture-buffer org-clock-marker)
                       (marker-buffer org-clock-marker)))
           (obj (org-dog-buffer-object buffer)))
      (when (and obj
                 (string-match-p (rx bol "projects/") (slot-value obj 'relative)))
        ;; Delay logging so it starts after magit commands finishes
        (run-with-timer 4 nil
                        `(lambda ()
                           (akirak-git-commit-log-to-org-clock-1
                            :marker ,(set-marker (make-marker)
                                                 (marker-position org-clock-marker)
                                                 buffer)
                            :dir ,(magit-toplevel)
                            :message ,(ring-ref log-edit-comment-ring 0)
                            :rev ,(magit-rev-parse "HEAD"))))))))

(cl-defun akirak-git-commit--build-log-line (&key time rev-link message)
  (declare (indent 0))
  (format-spec "%t %m (%r)"
               `((?t . ,(format-time-string (org-time-stamp-format t t) time))
                 (?m . ,(akirak-git-commit--first-line message))
                 (?r . ,rev-link))))

(defun akirak-git-commit--first-line (string)
  (car (split-string string "\n")))

(provide 'akirak-git-commit)
;;; akirak-git-commit.el ends here
