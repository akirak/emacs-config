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

(defun akirak-git-remote-permalink (filename start-line &optional end-line)
  (require 'browse-at-remote)
  ;; Based on browse-at-remote--file-url from browse-at-remote.el
  (let* ((remote-ref (browse-at-remote--remote-ref filename))
         (remote (car remote-ref))
         (target-repo (browse-at-remote--get-url-from-remote remote))
         (remote-type (browse-at-remote--get-remote-type (plist-get target-repo :unresolved-host)))
         (repo-url (plist-get target-repo :url))
         (url-formatter (browse-at-remote--get-formatter 'region-url remote-type)))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))
    (with-temp-buffer
      (unless (zerop (call-process "git" nil (list t nil) nil
                                   "--no-pager"
                                   "blame"
                                   "-L" (format "%d,%d"
                                                start-line
                                                (or end-line
                                                    start-line))
                                   "--line-porcelain"
                                   "--" (file-name-nondirectory filename)))
        (error "Git blame failed"))
      (goto-char (point-min))
      (let (entries)
        (while (< (point) (point-max))
          (push (akirak-git--parse-blame)
                entries))
        (let* ((max-time (seq-max (mapcar (lambda (ent)
                                            (alist-get 'committer-time ent))
                                          entries)))
               (entries1 (cl-member-if `(lambda (ent)
                                          (= (alist-get 'committer-time ent)
                                             ,max-time))
                                       entries))
               (offset (- (length entries)
                          (length entries1))))
          (pcase (car entries1)
            ((map rev filename original-lnum)
             (let* ((orig-start-line (- original-lnum
                                        offset))
                    (orig-end-line (1- (+ orig-start-line (length entries)))))
               (funcall url-formatter repo-url rev filename
                        orig-start-line
                        (unless (= orig-start-line orig-end-line)
                          orig-end-line))))))))))

(defun akirak-git--parse-blame ()
  "Parse the line porcelain format."
  (let (alist)
    (looking-at (rx bol (group (+ (any hex))) blank (group (+ digit))))
    (push (cons 'rev (match-string 1))
          alist)
    (push (cons 'original-lnum (string-to-number (match-string 2)))
          alist)

    (re-search-forward (rx bol "committer-time" blank
                           (group (+ digit))))
    (push (cons 'committer-time (string-to-number (match-string 1)))
          alist)

    (re-search-forward (rx bol "filename" blank
                           (group (+ nonl))))
    (push (cons 'filename (match-string 1))
          alist)

    (beginning-of-line 3)
    alist))

(provide 'akirak-git)
;;; akirak-git.el ends here
