;;; akirak-systemctl.el --- Interfacing with systemd -*- lexical-binding: t -*-

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

;;;; Transient

(defvar akirak-systemctl-unit-name nil)

(defvar akirak-systemctl-unit-active nil)

(defvar akirak-systemctl-root nil)

;;;###autoload (autoload 'akirak-systemctl "akirak-systemctl" nil 'interactive)
(transient-define-prefix akirak-systemctl (&optional root)
  [:description
   (lambda ()
     (format "Unit: %s (%s)" akirak-systemctl-unit-name akirak-systemctl-unit-active))
   :class transient-row
   ("s" "Start"
    (lambda ()
      (interactive)
      (akirak-systemctl--unit-sync "start"))
    :if-non-nil akirak-systemctl-unit-active)
   ("r" "Restart"
    (lambda ()
      (interactive)
      (akirak-systemctl--unit-sync "restart"))
    :if-non-nil akirak-systemctl-unit-active)
   ("s" "Stop"
    (lambda ()
      (interactive)
      (akirak-systemctl--unit-sync "stop"))
    :if-nil akirak-systemctl-unit-active)]
  (interactive "P")
  (setq akirak-systemctl-root root)
  (unless akirak-systemctl-unit-name
    (setq akirak-systemctl-unit-name
          (akirak-systemctl--complete-unit "Select a unit: " root)))
  (setq akirak-systemctl-unit-active
        (akirak-systemctl--active akirak-systemctl-unit-name root))
  (transient-setup 'akirak-systemctl))

;;;; Completion

(defun akirak-systemctl--complete-unit (prompt &optional root)
  (let ((alist (akirak-systemctl--list-units root)))
    (cl-labels
        ((annotator (candidate)
           (pcase (cdr (assoc candidate alist))
             ((map description)
              (concat " " (propertize description 'face 'font-lock-comment-face)))))
         (group (unit transform)
           (if transform
               unit
             (let ((props (cdr (assoc unit alist))))
               (concat (alist-get 'active props)
                       "/"
                       (alist-get 'sub props)))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'systemd-unit)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (completing-read prompt #'completions nil t))))

(defun akirak-systemctl--list-units (&optional root)
  (with-temp-buffer
    (let ((process-environment (cons "SYSTEMD_COLORS=false"
                                     process-environment))
          result)
      (unless (zerop (apply #'call-process "systemctl" nil (list t nil) nil
                            (append (list "-o" "json" "--no-pager" "--all")
                                    (unless root
                                      '("--user"))
                                    (list "list-units"))))
        (error "systemctl failed"))
      (goto-char (point-min))
      (mapcar (lambda (props)
                (cons (alist-get 'unit props)
                      props))
              (json-parse-buffer :object-type 'alist :array-type 'list)))))

;;;; Utilities

(defun akirak-systemctl--active (unit &optional root)
  (with-temp-buffer
    (let ((process-environment (cons "SYSTEMD_COLORS=false"
                                     process-environment)))
      (apply #'call-process "systemctl" nil (list t nil) nil
             (append (list "--no-pager")
                     (unless root
                       '("--user"))
                     (list "is-active" unit)))
      (string-trim (buffer-string)))))

(defun akirak-systemctl--unit-sync (subcommand)
  (with-temp-buffer
    (let ((process-environment (cons "SYSTEMD_COLORS=false"
                                     process-environment)))
      (if (zerop (apply #'call-process "systemctl" nil (list t nil) nil
                        (append (unless akirak-systemctl-root
                                  '("--user"))
                                (list subcommand akirak-systemctl-unit-name))))
          (message "systemctl succeeded")
        (message "systemctl failed")))))

;;;; Deprecated

;;;###autoload
(defun akirak-systemctl-toggle-unit (unit operation &optional root)
  (interactive (let ((root current-prefix-arg))
                 (pcase-exhaustive (akirak-systemctl--select-unit "Toggle systemd unit: "
                                                                  root)
                   (`(,name . ,(map :state))
                    (list name
                          (pcase state
                            ("enabled" "stop")
                            ("disabled" "start")
                            (_ (user-error "static unit")))
                          root)))))
  (when (yes-or-no-p (format "Are you sure you want to %s %s? " operation unit))
    (let ((default-directory (when root
                               "/sudo:localhost:/"))
          (buffer (generate-new-buffer "*systemctl*")))
      (with-existing-directory
        (make-process :name "systemctl"
                      :buffer buffer
                      :command `("systemctl"
                                 ,@(unless root '("--user"))
                                 ,operation ,unit)
                      :sentinel
                      `(lambda (process _event)
                         (when (eq 'exit (process-status process))
                           (if (= 0 (process-exit-status process))
                               (progn
                                 (message "systemctl successfully exited")
                                 (kill-buffer ,(buffer-name buffer)))
                             (message "systemctl unit %s failed: %s %s"
                                      ,unit
                                      (with-current-buffer ,(buffer-name buffer)
                                        (buffer-string)))))))))))

;;;###autoload
(defun akirak-systemctl-daemon-reload (&optional root)
  (interactive "P")
  (let ((default-directory (when root
                             "/sudo:localhost:/"))
        (buffer (generate-new-buffer "*systemctl*")))
    (with-existing-directory
      (make-process :name "systemctl"
                    :buffer buffer
                    :command `("systemctl"
                               ,@(unless root '("--user"))
                               "daemon-reload")
                    :sentinel
                    `(lambda (process _event)
                       (when (eq 'exit (process-status process))
                         (if (= 0 (process-exit-status process))
                             (progn
                               (message "systemctl daemons successfully reloaded")
                               (kill-buffer ,(buffer-name buffer)))
                           (message "systemctl daemon-reload failed: %s"
                                    (with-current-buffer ,(buffer-name buffer)
                                      (buffer-string))))))))))

(provide 'akirak-systemctl)
;;; akirak-systemctl.el ends here
