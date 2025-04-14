;;; akirak-pandoc.el --- Convenient utilities using pandoc -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025 Akira Komamura

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


(defcustom akirak-pandoc-executable "pandoc"
  ""
  :type 'file)

(defvar akirak-pandoc-input-formats nil)

;;;###autoload
(defun akirak-pandoc-save-as-org (begin end)
  "Convert the region to Org and save the result to kill ring."
  (interactive "r")
  (let ((format (or (akirak-pandoc-input-format)
                    (completing-read "Input format: "
                                     (akirak-pandoc-input-formats)
                                     nil t)))
        (outbuf (generate-new-buffer "*pandoc*"))
        (errfile (make-temp-file "pandoc-errors-")))
    (unwind-protect
        (progn
          (unless (zerop (call-process-region begin end akirak-pandoc-executable
                                              nil (list outbuf errfile)
                                              (concat "--from=" format)
                                              "--to=org"
                                              "-"))
            (error "pandoc failed: %s"
                   (with-temp-buffer
                     (insert-file-contents errfile)
                     (buffer-string))))
          (kill-new (with-current-buffer outbuf (buffer-string))))
      (delete-file errfile)
      (kill-buffer outbuf))))

;;;###autoload
(defun akirak-pandoc-replace-with-org (begin end)
  "Replace the region with an Org version of the content."
  (interactive "r" org-mode)
  (let ((format (if current-prefix-arg
                    (completing-read "Input format: "
                                     (akirak-pandoc-input-formats)
                                     nil t)
                  "gfm"))
        (errfile (make-temp-file "pandoc-errors-")))
    (unwind-protect
        (atomic-change-group
          (unless (zerop (call-process-region begin end akirak-pandoc-executable
                                              'delete (list t errfile) nil
                                              (concat "--from=" format)
                                              "--to=org" "-"))
            (error "pandoc failed: %s"
                   (with-temp-buffer
                     (insert-file-contents errfile)
                     (buffer-string)))))
      (delete-file errfile))))

(cl-defun akirak-pandoc-convert-string (string &key from to)
  (declare (indent 1))
  (let ((errfile (make-temp-file "pandoc-errors-")))
    (unwind-protect
        (with-temp-buffer
          (insert string)
          (unless (zerop (call-process-region
                          (point-min) (point-max)
                          akirak-pandoc-executable
                          'delete (list t errfile) nil
                          (concat "--from=" from)
                          (concat "--to=" to)
                          "-"))
            (error "pandoc failed: %s"
                   (with-temp-buffer
                     (insert-file-contents errfile)
                     (buffer-string))))
          (string-trim (buffer-string)))
      (delete-file errfile))))

(defun akirak-pandoc-input-format (&optional mode filename)
  "Return a pandoc input format for the buffer, if any."
  (let* ((mode (or mode major-mode))
         (filename (or filename (buffer-file-name (buffer-base-buffer)))))
    (if (and (eq mode 'markdown-mode)
             (and filename
                  (string-match-p "/README\\." filename)))
        "gfm"
      (car (member (string-remove-suffix "-mode" (symbol-name mode))
                   (akirak-pandoc-input-formats))))))

(defun akirak-pandoc-input-formats ()
  (or akirak-pandoc-input-formats
      (setq pandoc-input-formats
            (process-lines akirak-pandoc-executable "--list-input-formats"))))

(provide 'akirak-pandoc)
;;; akirak-pandoc.el ends here
