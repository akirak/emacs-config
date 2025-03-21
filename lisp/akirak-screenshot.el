;;; akirak-screenshot.el --- An integrated interface to screenshot commands -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

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


(require 'akirak-transient)

;;;; Backend configuration

(defcustom akirak-screenshot-wayshot-executable "wayshot"
  ""
  :type 'file)

;;;; Options

(defvar akirak-screenshot-delay-in-seconds nil)

(transient-define-infix akirak-screenshot-set-delay ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-screenshot-delay-in-seconds
  :zero-is-nil t
  :description "Delay in seconds")

(defun akirak-screenshot--maybe-delay ()
  (when akirak-screenshot-delay-in-seconds
    (message "Sleeping for %d seconds..." akirak-screenshot-delay-in-seconds)
    (sleep-for akirak-screenshot-delay-in-seconds)))

(defvar akirak-screenshot-cursor nil)

(transient-define-infix akirak-screenshot-toggle-cursor ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-screenshot-cursor
  :description "Cursor")

;;;; Suffixes

;;;; Predicates and dynamic descriptions

(defun akirak-screenshot--wayland-p ()
  "Non-nil if Wayshot (for Wayland) is available."
  (getenv "WAYLAND_DISPLAY"))

;;;; Prefix

;;;###autoload (autoload 'akirak-screenshot "akirak-screenshot" nil 'interactive)
(transient-define-prefix akirak-screenshot ()
  :description "Screenshot / screen recording"
  ["Options"
   ("-c" akirak-screenshot-toggle-cursor)
   ("-t" akirak-screenshot-set-delay)]
  ["Region"
   ;; ("rs" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Screen window"
   ;; ("ws" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Entire desktop"
   ;; ("ds" "Screenshot (Wayland)"
   ;;  :if akirak-screenshot--wayland-p)
   ]
  ["Emacs frame"
   ]
  (interactive)
  (when (and (akirak-screenshot--wayland-p)
             (not (executable-find akirak-screenshot-wayshot-executable)))
    (message "%s is not found in PATH" akirak-screenshot-wayshot-executable))
  (transient-setup 'akirak-screenshot))

(provide 'akirak-screenshot)
;;; akirak-screenshot.el ends here
