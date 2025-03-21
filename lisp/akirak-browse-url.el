;;; akirak-browse-url.el --- Convenient wrappers for browse-url -*- lexical-binding: t -*-

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


(defvar akirak-browser-url-orig-fn nil)

;;;###autoload
(defun akirak-browse-url-prefer-xwidget (url &optional arg)
  "Browse URL using xwidget-webkit when suitable.."
  (if (require 'xwidget nil t)
      (unless (akirak-browse-url--non-default-p url arg)
        (akirak-browse-url--xwidget url arg))
    (akirak-browse-url-external-browser url arg)))

(defun akirak-browse-url--xwidget (url &optional arg)
  ;; This is basically what `xwidget-webkit-goto-url' does, but reuse the
  ;; existing window if it is inside the same frame..
  (if-let* ((session (unless arg
                       (xwidget-webkit-current-session))))
      (progn
        (xwidget-webkit-goto-uri session url)
        (let ((buffer (xwidget-buffer (xwidget-webkit-current-session))))
          (if-let* ((window (get-buffer-window buffer)))
              (select-window window)
            ;; If you have any preference over window management, you
            (pop-to-buffer buffer))))
    (xwidget-webkit-new-session url)))

;;;###autoload
(defun akirak-browse-url-chromium-default (url &optional arg)
  (start-process "chromium"
                 nil
                 browse-url-chromium-program
                 ;; At present, only a single private profile is allowed.
                 ;; This may be changed in the future.
                 "--profile-directory=Default"
                 url))

;;;###autoload
(defun akirak-browse-url-private-url (url &optional arg)
  (start-process "chromium"
                 nil
                 browse-url-chromium-program
                 ;; At present, only a single private profile is allowed.
                 ;; This may be changed in the future.
                 "--profile-directory=Private"
                 url))

(defun akirak-browse-url-chromium-oneshot-advice (&rest _args)
  (advice-remove 'browse-url-chromium
                 #'akirak-browse-url-chromium-oneshot-advice)
  (setq browse-url-browser-function akirak-browser-url-orig-fn))

;;;###autoload
(defun akirak-browse-url-use-oneshot-chromium ()
  "Use Chromium for browsing the next url."
  (interactive)
  (setq akirak-browser-url-orig-fn browse-url-browser-function
        browse-url-browser-function #'browse-url-chromium)
  (advice-add 'browse-url-chromium
              :after #'akirak-browse-url-chromium-oneshot-advice))

(provide 'akirak-browse-url)
;;; akirak-browse-url.el ends here
