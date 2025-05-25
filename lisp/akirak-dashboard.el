;;; akirak-dashboard.el --- Custom building blocks for dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Akira Komamura

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


;;;; org-clock-history

(require 'dashboard)

(defun akirak-dashboard-org-clock-insert (list-size)
  (dashboard-insert-section
   "Org Clock (unfinished todo items)"
   (when (boundp 'org-clock-history)
     (akirak-dashboard-org-clock-items list-size))
   list-size
   'org-clock
   (dashboard-get-shortcut 'org-clock)
   `(lambda (&rest _)
      (akirak-dashboard-org-clock-open ',el))
   (akirak-dashboard-org-clock-format-item el)))

(defun akirak-dashboard-org-clock-items (list-size)
  ;; (require 'akirak-org-clock)
  (let (els)
    (catch 'dashboard-org-clock-finish
      (dolist (marker org-clock-history)
        (when (= list-size (length els))
          (throw 'dashboard-org-clock-finish t))
        (catch 'dashboard-org-clock-skip
          (when (and (markerp marker)
                     (buffer-live-p (marker-buffer marker)))
            (org-with-point-at marker
              (org-back-to-heading)
              (let ((el (org-element-at-point-no-context)))
                ;; Exclude items that I won't work on soon. If I need to work on
                ;; an item, it should have an active and unfinished todo keyword
                ;; and it should not have a future scheduled timestamp.
                (when (or (memq (org-element-property :todo-type el)
                                '(nil done))
                          (member (org-element-property :todo-keyword el)
                                  '("CASUAL"
                                    "STOPPED"))
                          (org-element-property :archivedp el)
                          (when-let* ((ts (org-element-property :scheduled el)))
                            (time-less-p (current-time) (org-timestamp-to-time ts))))
                  (throw 'dashboard-org-clock-skip t))
                (push (thread-first
                        el
                        (org-element-put-property :category (org-entry-get nil "CATEGORY" t))
                        (org-element-put-property :hd-marker (point-marker)))
                      els)))))))
    (nreverse els)))

(defun akirak-dashboard-org-clock-format-item (el)
  (concat (if-let* ((ts (org-element-property :deadline el)))
              (concat "D " (org-element-property :raw-value ts))
            ;; Take the value from an internal convention for any active
            ;; timestamp.
            (if-let* ((ts (org-element-property :active-timestamp el)))
                ;; TODO: Use a human-friendly format.
                (concat (akirak-dashboard--format-org-ts ts)
                        " ")
              ""))
          (if-let* ((prio (org-element-property :priority el)))
              (propertize (format "#%s " (char-to-string prio))
                          'face 'org-priority)
            "")
          (if-let* ((kwd (org-element-property :todo-keyword el)))
              (concat (propertize kwd 'face (or (org-get-todo-face kwd) 'default))
                      " ")
            "")
          (format "[%s] "
                  (or (org-element-property :category el)
                      "?"))
          (org-link-display-format (org-element-property :raw-value el))))

(defun akirak-dashboard--format-org-ts (ts)
  "Format an Org timestamp."
  (let ((time (org-timestamp-to-time ts))
        (decoded-today (akirak-dashboard--decoded-today)))
    (if (time-less-p (org-timestamp-to-time (akirak-dashboard--fill-org-ts ts))
                     (encode-time decoded-today))
        (propertize (org-format-timestamp ts "%Y-%m-%d (%a)")
                    'face 'org-imminent-deadline)
      (pcase (- (time-to-days time)
                (time-to-days (current-time)))
        ;; future
        (1
         "tomorrow")
        ;; today
        (0
         (if (equal (list 0 0 org-extend-today-until)
                    (list 0 0 (org-element-property :hour-start ts)))
             (propertize "today" 'face 'org-scheduled-today)
           (let ((diff (/ (float-time (time-subtract time (current-time)))
                          60)))
             (if (< diff 0)
                 (propertize (format "in %s" (org-duration-from-minutes diff))
                             'face 'org-imminent-deadline)
               (propertize (format "in %s" (org-duration-from-minutes diff))
                           'face 'org-upcoming-deadline)))))
        ;; yesterday
        (-1
         (propertize "yesterday"
                     'face 'org-imminent-deadline))))))

(defun akirak-dashboard--fill-org-ts (ts)
  (let ((hour (or (org-element-property :hour-start ts)
                  org-extend-today-until))
        (minute (or (org-element-property :minute-start ts)
                    0))
        (second (or (org-element-property :second-start ts)
                    0)))
    (thread-first
      (copy-sequence ts)
      (org-element-put-property :hour-start hour)
      (org-element-put-property :minute-start minute)
      (org-element-put-property :second-start second))))

(defun akirak-dashboard-org-clock-open (el)
  (require 'org-dog)
  (with-current-buffer (org-dog-indirect-buffer
                        (or (org-element-property :hd-marker el)
                            ;; Convention used by org-ql.
                            (org-element-property :org-hd-marker el)))
    (run-hooks 'akirak-org-clock-open-hook)
    (pop-to-buffer (current-buffer))))

;;;; A custom implementation of org-agenda

(defun akirak-dashboard-insert-agenda (list-size)
  (dashboard-insert-section
   "Scheduled for today"
   (akirak-dashboard--agenda-items 'day)
   list-size
   'custom-agenda
   (dashboard-get-shortcut 'custom-agenda)
   `(lambda (&rest _)
      (akirak-dashboard-org-clock-open ',el))
   (akirak-dashboard-org-clock-format-item el)))

(defun akirak-dashboard--decoded-today ()
  (let ((decoded-now (decode-time)))
    (append (list 0 0 org-extend-today-until)
            (cdddr (if (and org-extend-today-until
                            (< (nth 2 decoded-now) org-extend-today-until))
                       (decoded-time-add decoded-now (make-decoded-time :day -1))
                     decoded-now)))))

(defun akirak-dashboard--agenda-items (span)
  (require 'org-ql)
  (let* ((today (akirak-dashboard--decoded-today))
         (dates (list today
                      (decoded-time-add today (make-decoded-time :day 1))))
         (regexp (rx-to-string `(and "<"
                                     (or ,@(mapcar (lambda (day)
                                                     (format-time-string "%F" (encode-time day)))
                                                   dates))
                                     (repeat 0 16 (not (any ">\n")))
                                     ">")))
         (files (seq-filter (lambda (file)
                              (if-let* ((buffer (org-find-base-buffer-visiting file)))
                                  (with-current-buffer buffer
                                    (org-with-wide-buffer
                                     (goto-char (point-min))
                                     (re-search-forward regexp nil t)))
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (goto-char (point-min))
                                  (re-search-forward regexp nil t))))
                            (org-dog-select 'absolute))))
    ;; Update `org-agenda-files'.
    (when files
      (setq org-agenda-files (seq-union files org-agenda-files))
      (thread-last
        (org-ql-select files
          `(and (regexp ,regexp)
                (not (done))
                (not (tags "ARCHIVE")))
          :action `(lambda ()
                     (save-excursion
                       (let* ((el (org-element-at-point-no-context))
                              (bound (org-element-end el))
                              timestamps)
                         (when (re-search-forward ,regexp bound t)
                           (push (org-timestamp-from-string (match-string 0))
                                 timestamps))
                         (thread-first
                           el
                           (org-element-put-property :category (org-entry-get nil "CATEGORY" t))
                           (org-element-put-property :hd-marker (point-marker))
                           (org-element-put-property :active-timestamp
                                                     (thread-last
                                                       timestamps
                                                       (seq-sort-by #'org-timestamp-to-time
                                                                    #'time-less-p)
                                                       (car))))))))
        (seq-sort-by (lambda (el)
                       (org-element-property :todo-keyword el))
                     #'string-lessp)
        (seq-sort-by (lambda (el)
                       (or (org-element-property :priority el)
                           org-default-priority))
                     #'<)
        (seq-sort-by (lambda (el)
                       (org-timestamp-to-time (org-element-property :active-timestamp el)))
                     #'time-less-p)))))

(provide 'akirak-dashboard)
;;; akirak-dashboard.el ends here
