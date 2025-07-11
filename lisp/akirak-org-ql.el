;;; akirak-org-ql.el --- org-ql extensions -*- lexical-binding: t -*-

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


(require 'org-ql)

(require 'org-dog)
(require 'org-reverse-datetree)

(org-ql-defpred dogm ()
  "Filter entries that are meaningful per org-dog scheme."
  :body (if-let* ((obj (org-dog-buffer-object)))
            (org-dog-meaningful-in-file-p obj)
          t))

(org-ql-defpred (pr project) ()
  "Filter entries that are related to the current project."
  :normalizers ((`(,predicate-names)
                 (when-let* ((pr (project-current)))
                   (let ((root (abbreviate-file-name (project-root pr)))
                         (origin (ignore-errors
                                   ;; TODO: Break the host and path
                                   (car (magit-config-get-from-cached-list
                                         "remote.origin.url")))))
                     `(or (regexp ,(regexp-quote root))
                          (property "GIT_ORIGIN" ,origin :inherit t))))))
  :body t)

(org-ql-defpred (br git-branch) ()
  "Filter entries that explicitly refer to the current branch of the project."
  :normalizers ((`(,predicate-names)
                 (when-let* ((branch (magit-get-current-branch)))
                   `(and (project)
                         (regexp ,(regexp-quote branch))))))
  :body t)

(org-ql-defpred (cl my-clocked) (&optional arg)
  "A convenient version of `clocked' predicate "
  :normalizers ((`(,predicate-names)
                 '(clocked :from -3))
                (`(,predicate-names ,arg)
                 (pcase arg
                   ((pred (string-match-p (rx bol (+ digit) eol)))
                    `(clocked :from ,(- (string-to-number arg))))
                   (_
                    '(clocked :from -3)))))
  :body t)

(org-ql-defpred datetree ()
  "Return non-nil if the entry is a direct child of a date entry."
  :body
  (org-reverse-datetree-date-child-p))

(org-ql-defpred archived ()
  "Return non-nil if the entry is archived."
  :body
  (org-in-archived-heading-p))

(org-ql-defpred recur ()
  "Return non-nil if the entry has an `org-recur' annotation"
  :body
  (org-recur--get-next-date
   (org-get-heading t t t t)))

(org-ql-defpred edna-blocked ()
  "Return non-nil if the entry is blocked by org-edna."
  :body
  (let ((org-blocker-hook '(org-edna-blocker-function)))
    (org-entry-blocked-p)))

(defmacro akirak-org-ql-define-todo-predicates ()
  (let ((kwds (with-temp-buffer
                (let ((org-inhibit-startup t))
                  (delay-mode-hooks (org-mode))
                  org-todo-keywords-1)))
        (existing-names (mapcar #'car org-ql-predicates)))
    (cl-flet
        ((names-for-kwd (kwd)
           (cl-set-difference
            (list (intern (downcase kwd))
                  (intern (downcase (substring kwd 0 3))))
            existing-names
            :test #'eq)))
      `(let ((byte-compile-warnings nil))
         (org-ql-defpred (my-todo ,@(mapcan #'names-for-kwd kwds))
           ()
           "Filter entries with a todo keyword."
           :normalizers
           ,(mapcar (lambda (kwd)
                      `(`(,(or ,@(mapcar (lambda (sym)
                                           (list 'quote sym))
                                         (names-for-kwd kwd))))
                        '(todo ,kwd)))
                    kwds)
           :body t)))))

;;;###autoload
(defun akirak-org-ql-open-link (files)
  "Open a link at a heading from FILES."
  (require 'org-pivot-search)
  (pcase (org-pivot-search-from-files files
           :prompt "Open link: "
           :query-prefix (concat org-pivot-search-query-prefix
                                 " "
                                 (format "heading-regexp:%s "
                                         ;; org-link-any-re contains a space, which makes it unsuitable
                                         ;; for use in non-sexp org-ql queries.
                                         (rx-to-string `(or (and "http" (?  "s") ":")
                                                            (regexp ,org-link-bracket-re)))))
           :interactive nil)
    (`(,_ . ,input)
     (if-let* ((marker (get-text-property 0 'org-marker input)))
         (org-with-point-at marker
           (org-back-to-heading)
           (org-match-line org-complex-heading-regexp)
           (goto-char (match-beginning 4))
           (org-open-at-point))
       (error "No marker on input")))))

(provide 'akirak-org-ql)
;;; akirak-org-ql.el ends here
