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


(defun akirak-systemctl--list-unit-files (&optional root)
  (with-temp-buffer
    (let ((process-environment (cons "SYSTEMD_COLORS=false"
                                     process-environment))
          result)
      (call-process "systemctl" nil (list t nil) nil
                    "--user" "list-unit-files" "--no-pager")
      (goto-char (point-min))
      ;; Ensure the output follows an expected format.
      (unless (looking-at (rx "UNIT FILE" (+ blank)
                              "STATE" (+ blank)
                              "PRESET" eol))
        (error "The output format of systemctl seems to have changed"))
      (forward-line)
      (while (and (not (eolp))
                  (looking-at (rx (group (+? anything))
                                  (+ blank) (group (+? anything))
                                  (+ blank) (group (+? anything))
                                  eol)))
        (push (mapcar #'match-string
                      (number-sequence 1 (1- (/ (length (match-data)) 2))))
              result)
        (forward-line))
      (mapcar (pcase-lambda (`(,name ,state ,preset))
                (list name :state state :preset preset))
              result))))

(defun akirak-systemctl--select-unit (prompt &optional root)
  (let ((alist (akirak-systemctl--list-unit-files root)))
    (cl-labels
        ((annotator (candidate)
           (pcase (cdr (assoc candidate alist))
             ((map :state)
              (format " %s" state))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'systemd-unit)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (assoc (completing-read prompt #'completions nil t)
             alist))))

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
