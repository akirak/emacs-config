;;; akirak-log.el ---  -*- lexical-binding: t -*-

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


(defcustom akirak-log-private-file nil
  ""
  :type '(choice file (const nil)))

(defmacro akirak-log-with-wide-buffer (&rest body)
  `(with-current-buffer (or (org-find-base-buffer-visiting akirak-log-private-file)
                            (find-file-noselect akirak-log-private-file))
     (org-with-wide-buffer
      ,@body)))

;;;###autoload
(define-minor-mode akirak-log-mode
  "Global minor mode that logs certain events to an Org file."
  :global t
  (when akirak-log-mode
    (if (and akirak-log-private-file
             (file-writable-p akirak-log-private-file))
        (let ((dir (file-name-directory akirak-log-private-file)))
          (unless (file-directory-p dir)
            (make-directory dir t)))
      (setq akirak-log-mode nil)))
  (funcall (if akirak-log-mode
               #'add-hook
             #'remove-hook)
           'find-file-hook #'akirak-log--find-file)
  (funcall (if akirak-log-mode
               #'add-hook
             #'remove-hook)
           'magit-status-mode-hook #'akirak-log--project))

(defun akirak-log--enabled-p ()
  (and akirak-log-private-file
       (not (and (fboundp 'org-clocking-p)
                 (org-clocking-p)))))

(cl-defun akirak-log--heading (heading &key time tags message)
  (if (and akirak-log-private-file
           (file-readable-p akirak-log-private-file))
      (with-current-buffer (or (org-find-base-buffer-visiting akirak-log-private-file)
                               (find-file-noselect akirak-log-private-file))
        (org-with-wide-buffer
         (goto-char (point-max))
         (insert (format-spec "* %t %h%g
:PROPERTIES:
:emacs_pid:  %p
:login:      %l
:exact_time: %u
:END:\n"
                              `((?t . ,(format-time-string
                                        (org-time-stamp-format t t)
                                        time))
                                (?u . ,(format-time-string "%FT%H:%M:%S%:z"))
                                (?h . ,heading)
                                (?g . ,(if tags
                                           (concat " "
                                                   (org-make-tag-string
                                                    (ensure-list tags)))
                                         ""))
                                (?p . ,(emacs-pid))
                                (?l . ,(concat (user-login-name) "@" (system-name)))))
                 (when message
                   (string-chop-newline message)))
         (when message
           (insert-char ?\n))))
    (user-error "akirak-log-private-file is nil or not readable")))

(defun akirak-log--format-file-name (file)
  ;; TODO: Prevent loading ol for a shorter initial response time?
  (require 'ol)
  (org-link-make-string
   (concat "file:" (abbreviate-file-name file))))

(defun akirak-log--find-file ()
  (when (and (or (memq current-minibuffer-command '(consult-recent-file
                                                    consult-buffer))
                 (and (buffer-file-name)
                      (not (file-exists-p (buffer-file-name)))))
             (akirak-log--enabled-p))
    (akirak-log--heading (akirak-log--format-file-name (buffer-file-name))
                         :tags "file"
                         :message "Visit a recent file.")))

(defun akirak-log--project ()
  ;; None of `current-minibuffer-command', `this-command' and its alternatives
  ;; can detect invocation of `akirak-consult-dir' or `akirak-project-switch',
  ;; so use (not (eq this-command 'magit-status)) here.
  (when (and (not (eq this-command 'magit-status))
             (akirak-log--enabled-p))
    (akirak-log--heading (akirak-log--format-file-name default-directory)
                         :tags "project"
                         :message "Switch to a project.")))

;;;###autoload
(defun akirak-log-memento-activities (start-time end-time)
  (when (file-readable-p akirak-log-private-file)
    (let (result
          (start-time-float (float-time start-time))
          (end-time-float (float-time end-time)))
      (with-current-buffer (or (find-buffer-visiting akirak-log-private-file)
                               (find-file-noselect akirak-log-private-file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward (rx bol "* ") nil t)
           (when (looking-at org-ts-regexp-inactive)
             (let ((time (thread-last
                           (save-match-data
                             (if-let* ((string (save-match-data
                                                 (org-entry-get nil "exact_time"))))
                                 (parse-iso8601-time-string string)
                               (encode-time (parse-time-string (match-string 1)))))
                           (float-time))))
               (when (and (>= time start-time-float)
                          (< time end-time-float))
                 (goto-char (match-end 0))
                 (if (org-match-line org-complex-heading-regexp)
                     (let* ((rem-headline (buffer-substring-no-properties
                                           (point) (match-end 4)))
                            (link (when (string-match org-link-bracket-re
                                                      rem-headline)
                                    (match-string 1 rem-headline))))
                       (push (list time time
                                   (or link rem-headline)
                                   (copy-marker (line-beginning-position))
                                   'event)
                             result))
                   (error "Failed to match on line: %s" (point-marker)))))))))
      result)))

;;;###autoload
(cl-defun akirak-log-browse-url (url &key effort tags)
  (interactive (akirak-log--complete-url-with-effort "Browse url: "))
  (unless (and akirak-log-private-file
               (file-writable-p akirak-log-private-file))
    (user-error "akirak-log-private-file is nil or not writable"))
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :file ,akirak-log-private-file
                  :template ,(akirak-org-capture-make-entry-body
                               url
                               :tags tags
                               :properties `(("Effort" . ,effort))
                               :log-time nil
                               :body t)
                  :clock-in t :clock-resume t))))))
    (browse-url url)
    (org-capture)))

(defun akirak-log--complete-url-with-effort (prompt)
  (let* ((alist (akirak-log--url-activities))
         (urls (seq-uniq (mapcar #'car alist))))
    (cl-labels
        ((annotator (candidate)
           (let* ((date (plist-get (cdr (assoc candidate alist))
                                   :date))
                  (date-diff (when date
                               (floor (/ (- (float-time)
                                            (float-time (encode-time date)))
                                         86400)))))
             (when date
               (format " (last activity %s)"
                       (pcase date-diff
                         (0 "today")
                         (1 "yesterday")
                         (_ (format "%d days ago" date-diff)))))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'category)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action urls string pred))))
      (let* ((url (completing-read prompt #'completions))
             (efforts (thread-last
                        alist
                        (seq-filter `(lambda (cell)
                                       (equal ,url (car cell))))
                        (mapcar (lambda (cell)
                                  (plist-get (cdr cell) :effort)))
                        (seq-uniq)))
             (effort (completing-read "Effort: " efforts))
             (tags (completing-read-multiple
                    "Tags: "
                    (flatten-tree (org-tag-alist-to-groups org-tag-persistent-alist))
                    nil nil
                    (string-join (plist-get (cdr (assoc url alist))
                                            :tags)
                                 crm-separator))))
        (list url :effort effort :tags tags)))))

(defun akirak-log--url-activities ()
  (akirak-log-with-wide-buffer
   (let ((regexp (format org-complex-heading-regexp-format org-link-plain-re))
         result)
     (goto-char (point-min))
     (while (re-search-forward regexp nil t)
       (push (list (match-string-no-properties 4)
                   :tags (org-get-tags)
                   :effort (org-entry-get nil "Effort")
                   :clock-sum (org-clock-sum-current-item)
                   :date (when (re-search-forward org-ts-regexp-inactive
                                                  (org-entry-end-position)
                                                  t)
                           (parse-time-string (match-string 1))))
             result))
     result)))

(provide 'akirak-log)
;;; akirak-log.el ends here
