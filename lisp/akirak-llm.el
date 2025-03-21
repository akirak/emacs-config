;;; akirak-llm.el --- Various extra commands for LLM -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

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


(defcustom akirak-llm-gptel-programming-model 'claude-3-5-sonnet-20241022
  ""
  :type 'symbol)

;;;###autoload
(defun akirak-llm-browse-package-info (package)
  (interactive "sPackage: ")
  (let ((prompt (format "Please find web pages on %s package defined in a %s file."
                        package
                        (file-name-nondirectory (buffer-file-name))))
        (gptel-model akirak-llm-gptel-programming-model)
        (gptel-backend (akirak-llm-gptel-find-backend-for-model gptel-model)))
    (message "Querying...")
    (gptel-request prompt
      :system "You are a software developer proficient with many programming languages. \
Please return the answer in a JSON array of objects with title and url keys."
      :callback (apply-partially #'akirak-llm-handle-web-page-list
                                 (format "Browse package %s: " package)))))

(defun akirak-llm-gptel-find-backend-for-model (model)
  (seq-some `(lambda (cell)
               (let ((backend (cdr cell)))
                 (when (memq ',model (gptel-backend-models backend))
                   backend)))
            gptel--known-backends))

(defun akirak-llm-handle-web-page-list (prompt response _info)
  (with-temp-buffer
    (insert response)
    (goto-char (point-min))
    (akirak-llm-select-url-to-browse
     prompt
     (json-parse-buffer :array-type 'list :object-type 'alist))))

(defun akirak-llm-select-url-to-browse (prompt alists)
  (let ((tbl (mapcar (lambda (alist)
                       (cons (alist-get 'url alist)
                             (alist-get 'title alist)))
                     alists)))
    (cl-labels
        ((annotator (candidate)
           (concat " " (cdr (assoc candidate tbl))))
         (completion (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'url)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action tbl string pred))))
      (dolist (url (ensure-list
                    (completing-read-multiple prompt
                                              #'completion
                                              nil t)))
        (browse-url url)))))

(provide 'akirak-llm)
;;; akirak-llm.el ends here
