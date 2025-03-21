;;; akirak-hyprland.el ---  -*- lexical-binding: t -*-

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


(defcustom akirak-hyprland-hyprctl-executable
  "hyprctl"
  ""
  :type 'file)

(cl-defun akirak-hyprland-read-window (prompt)
  (let ((alist (thread-last
                 (akirak-hyprland--hyprctl "clients")
                 (mapcar (lambda (x)
                           (cons (alist-get 'title x)
                                 x))))))
    (cl-labels
        ((annotator (candidate)
           (thread-last
             (assoc candidate alist)
             (cdr)
             (alist-get 'class)
             (concat " ")))
         (group (candidate transform)
           (when transform
             (thread-last
               (assoc candidate alist)
               (cdr)
               (alist-get 'workspace)
               (alist-get 'name))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'hyprland-window)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (completing-read prompt #'completions nil t))))

(defun akirak-hyprland--hyprctl (command)
  (with-temp-buffer
    (unless (zerop (call-process akirak-hyprland-hyprctl-executable nil (list t nil) nil
                                 command "-j"))
      (error "hyprctl returned non-zero"))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(provide 'akirak-hyprland)
;;; akirak-hyprland.el ends here
