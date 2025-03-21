;;; akirak-mime.el ---  -*- lexical-binding: t -*-

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


(defcustom akirak-mime-globs-file
  (when (eq system-type 'gnu/linux)
    (format "/etc/profiles/per-user/%s/share/mime/globs"
            (user-login-name)))
  ""
  :type '(choice file
                 (const nil)))

;;;###autoload
(defun akirak-mime-set-mime-extensions ()
  (when (and akirak-mime-globs-file
             (file-readable-p akirak-mime-globs-file))
    (with-temp-buffer
      (insert-file-contents akirak-mime-globs-file)
      (goto-char (point-min))
      (let (result)
        (while (re-search-forward (rx bol (group
                                           (not (any "#"))
                                           (* (not (any ":"))))
                                      ;; Support only simple file patterns
                                      ":*" (group "." (+ (any "-+." alnum)))
                                      eol)
                                  nil t)
          (push (cons (match-string-no-properties 2)
                      (match-string-no-properties 1))
                result))
        (setq mailcap-mime-extensions (nreverse result)
              ;; Prevent from being overridden by the built-in function
              mailcap-mimetypes-parsed-p t
              mailcap-parsed-p t)))))

(defun akirak-mime-matching-suffixes (mime-regexp)
  (thread-last
    (cl-remove-if-not `(lambda (mime)
                         (string-match-p ,(concat "^" mime-regexp "$") mime))
                      mailcap-mime-extensions
                      :key #'cdr)
    (mapcar #'car)))

;;;###autoload
(defun akirak-mime-update-org-file-apps ()
  "Update `org-file-apps' from the user mime extensions."
  (require 'mailcap)
  (akirak-mime-set-mime-extensions)
  (dolist (x mailcap-user-mime-data)
    (let ((test (cdr (assq 'test x)))
          (suffixes (akirak-mime-matching-suffixes (cdr (assq 'type x))))
          (viewer (cdr (assq 'viewer x))))
      (when (and suffixes
                 (functionp viewer)
                 (or (not test)
                     (eval test)))
        (cl-pushnew (cons (rx-to-string `(and (or ,@suffixes)
                                              eol))
                          `(lambda (file _)
                             (funcall ',viewer file)))
                    org-file-apps)))))

(provide 'akirak-mime)
;;; akirak-mime.el ends here
