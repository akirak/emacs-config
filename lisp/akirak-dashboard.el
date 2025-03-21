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
                        (org-element-put-property :CATEGORY (org-entry-get nil "CATEGORY" t))
                        (org-element-put-property :hd-marker (point-marker)))
                      els)))))))
    (nreverse els)))

(defun akirak-dashboard-org-clock-format-item (el)
  (concat (if-let* ((kwd (org-element-property :todo-keyword el)))
              (concat (org-no-properties kwd) " ")
            "")
          (format "[%s] "
                  (or (org-element-property :CATEGORY el)
                      "?"))
          (org-link-display-format (org-element-property :raw-value el))
          (if-let* ((ts (org-element-property :deadline el)))
              (concat " D" (org-element-property :raw-value ts))
            "")))

(provide 'akirak-dashboard)
;;; akirak-dashboard.el ends here
