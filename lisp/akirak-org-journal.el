;;; akirak-org-journal.el --- Utilities for org-journal -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/trivial-elisps

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

;; This is a collection of helper libraries for Org mode.

;;; Code:

(require 'org)
(require 'org-journal)

(defvar org-agenda-window-setup)

(defvar akirak-org-journal-current-file nil
  "The current journal entry.")

;;;###autoload
(defun akirak-org-journal-setup ()
  "Set up advices and hooks `org-journal'."
  ;; (when (eq org-journal-file-type 'weekly)
  ;;   (setq org-journal-file-header
  ;;         (defun akirak/org-journal-weekly-header (time)
  ;;           (format-time-string "#+TITLE: Week %-W, %Y" time))))
  (advice-add #'org-journal-next-entry
              :after #'akirak-org-journal--overview)
  (advice-add #'org-journal-previous-entry
              :after #'akirak-org-journal--overview)
  (add-to-list 'recentf-exclude #'akirak-org-journal-file-p t))

(defun akirak-org-journal-file-p (file)
  (ignore-errors
    (and (string-prefix-p (expand-file-name org-journal-dir)
                          (expand-file-name file))
         (string-suffix-p ".org" file))))

;;;###autoload
(defun akirak-org-journal-find-location (&optional time)
  "Go to the beginning of the journal file.

This can be used for defining an `org-capture' template to create
an entry in the journal.

Optionally, you can specify TIME for pointoing to a particular
date, which is given to `org-journal-new-entry'."
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t time)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (widen)
  (re-search-backward (rx bol "* "))
  (org-show-entry))

;;;###autoload
(cl-defun akirak-org-journal-ensure-group (heading &key tags todo)
  "Go to a particular group in the journal.

This moves the point to a group with a HEADING. If the heading
does not , it is created.

Optionally, you can specify TAGS which is added to the heading.

If TODO is non-nil, it is inserted into the heading. It can be
either t or a string."
  (declare (indent 1))
  (akirak-org-journal-find-location)
  (let ((end (save-excursion
               (org-end-of-subtree 'invisible))))
    (if (re-search-forward (rx-to-string `(and bol "**" (+ space)
                                               (? (regexp ,org-todo-regexp)
                                                  (+ space))
                                               ,heading))
                           end t)
        (end-of-line)
      (goto-char end)
      (progn
        (just-one-space -1)
        (delete-horizontal-space)
        (insert "\n** "
                (if todo
                    (concat (if (stringp todo)
                                todo
                              "TODO")
                            " ")
                  "")
                heading))
      (when tags
        (org-set-tags tags))
      (org-end-of-line))))

;;;###autoload
(defun akirak-org-journal-overview (&optional arg)
  "Go to the current date in the journal and display an overview.

If ARG is non-nil, it also updates a dynamic block immediately
after the heading, if any.

If the point is already in the designated location, it will open
`org-agenda' in another window."
  (interactive "P")
  (let ((point (point))
        (initial-buffer (buffer-name)))
    (org-journal-new-entry t)
    (re-search-backward (rx bol "* "))
    (if (and (equal initial-buffer (buffer-name))
             (equal point (point)))
        (let ((org-agenda-window-setup 'other-window))
          (org-agenda nil "a"))
      (akirak-org-journal--overview)
      ;; I have a dynamic block for time tracking in each journal.
      ;; I want to update its contents when I review the journal.
      (when arg
        (save-excursion
          (forward-line)
          (when (looking-at org-dblock-start-re)
            (org-dblock-update)))))))

(defun akirak-org-journal--overview ()
  "Display an overview of the current entry."
  ;; `org-journal--finalize-view' sets the heading visibility,
  ;; which I don't want because it is redundant.
  ;; There is no way to avoid that at present.
  (org-content 3)
  (org-show-set-visibility 'local))

;;;###autoload
(defun akirak-org-journal-refile-to-date (time)
  (interactive (list (org-read-date nil 'to-time)))
  (cond
   ((derived-mode-p 'org-mode)
    (let* ((orig-marker (prog1 (point-marker)
                          (akirak-org-journal-find-location time)))
           (filename (buffer-file-name))
           (rfloc (list (nth 4 (org-heading-components))
                        filename
                        nil
                        (point))))
      (with-current-buffer (marker-buffer orig-marker)
        (goto-char (marker-position orig-marker))
        (org-refile nil nil rfloc))))))

(defalias 'akirak-org-journal-files 'org-journal--list-files
  "Return a list of journal files in `org-journal-dir'.")

(defun akirak-org-journal-current-file ()
  (ignore-errors
    (setq akirak-org-journal-current-file
          (org-journal--get-entry-path (current-time)))))

(provide 'akirak-org-journal)
;;; akirak-org-journal.el ends here
