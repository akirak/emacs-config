;;; akirak-url.el --- A library for URL patterns -*- lexical-binding: t -*-

;; Copyright (C) 2021 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/trivial-elisps

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

;; This library provides constants and functions related to some variations of
;; URLs.

;;; Code:

(declare-function org-link-make-string "ol")
(declare-function akirak-org-capture-make-entry-body "akirak-org-capture")
(declare-function orgabilize-insert-org-link "orgabilize")

;;;###autoload (autoload 'orgabilize-document-title "orgabilize-document")

(defgroup akirak-url nil
  "Utilities for URLs."
  :prefix "akirak"
  :group 'akirak)

;;;; Processing URLs: Matching, sanitizing, etc.

;; Taken from my clipurl.el.
;; <https://github.com/akirak/clipurl.el/blob/master/clipurl.el>
(eval-and-compile
  (defconst akirak-url--html-xalpha
    ;; TODO: Add thorough tests and fix this pattern
    (let* ((safe "-$=_@.&+")
           (extra "!*(),~")
           ;; I don't think people would want URLs containing
           ;; double/single quotes, but the spec contains them.
           ;;
           ;; (extra "!*\"'(),")
           (escape '(and "%" (char hex) (char hex))))
      `(or ,escape (char alpha digit ,safe ,extra))))

  (defconst akirak-url--html-host-pattern
    (let* ((xalpha akirak-url--html-xalpha)
           (ialpha `(and (char alpha) (* ,xalpha)))
           (hostname `(and ,ialpha (* (and "." ,ialpha))))
           (hostnumber '(and (+ (char digit))
                             (repeat 3 (and "." (+ (char digit)))))))
      `(or ,hostname ,hostnumber)))

  (defconst akirak-url-html-regexp
    (rx "http" (?  "s")
        "://"
        (eval akirak-url--html-host-pattern)
        (?  ":" (+ (char digit)))
        (?  (or (and (+ "/" (+ (eval akirak-url--html-xalpha)))
                     (?  "/"))
                "/"))
        (?  "?" (and (+ (eval akirak-url--html-xalpha))
                     (* "+" (+ (eval akirak-url--html-xalpha)))))
        (?  "#" (+ (or (+ (eval akirak-url--html-xalpha))
                       (char "/?")))))))

(defun akirak-url--match-html-string (string)
  "Return the first match of a web page url in STRING, if any."
  (save-match-data
    (when (string-match akirak-url-html-regexp string)
      (match-string 0 string))))

(defun akirak-url--clean (url)
  "Remove undesired parameters from URL, if any."
  (save-match-data
    (if-let (start (string-match (rx (group (any "?&"))
                                     (group "utm_source=" (+ (not (any "&"))))
                                     (group (? "&")))
                                 url))
        (concat (substring url 0 start)
                (if (equal (match-string 3 url) "&")
                    (concat (match-string 1 url)
                            (substring url (nth 1 (match-data))))
                  ""))
      url)))

;;;; Completing URLs

;;;###autoload
(defun akirak-url-complete (prompt &optional initial-input history)
  "Complete a url from the clipboard with PROMPT."
  (completing-read prompt
                   (akirak-url--completions (akirak-url--recent-kills))
                   nil nil initial-input history))

(defun akirak-url--completions (urls)
  "Return a completion table for URLS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata . ((category . url)))
      (complete-with-action action urls string pred))))

;;;;; Recent URLs from the kill ring

(defcustom akirak-url-max-recent-items 5
  ""
  :type 'number)

(defun akirak-url--recent-kills (&optional max)
  (let ((i 0)
        (n 0)
        (len (length kill-ring))
        result)
    (catch 'finish
      (while (and (< i len)
                  (< n (or max akirak-url-max-recent-items)))
        (if-let (k (current-kill i t))
            (when-let (url (akirak-url--match-html-string k))
              (unless (member url result)
                (push (akirak-url--clean (substring-no-properties url))
                      result)
                (cl-incf n)))
          (throw 'finish t))
        (cl-incf i)))
    (nreverse result)))

;;;; Inserting a link to a URL

(defvar akirak-url-insert-history nil)

;;;###autoload
(defun akirak-url-insert-dwim (url &optional arg)
  "Insert a link to URL suitable for the major mode.

With a prefix argument, insert a link with the fragment title.
This is only supported in `org-mode' at present."
  (interactive (list (akirak-url-complete "URL to insert: " nil
                                          akirak-url-insert-history)
                     current-prefix-arg))
  (cond
   ((derived-mode-p 'org-mode)
    (orgabilize-insert-org-link url arg))
   ((derived-mode-p 'markdown-mode)
    (akirak-url-insert-as-markdown url))
   (t (insert url))))

;;;###autoload
(defun akirak-url-insert-as-markdown (url)
  "Insert a link to URL in markdown format, whatever the major mode is."
  (interactive (list (akirak-url-complete "URL to insert: " nil
                                          akirak-url-insert-history)))
  (insert (format "[%s](%s)"
                  (or (orgabilize-document-title url)
                      (read-string "Title for the URL: "))
                  url)))

(defcustom akirak-url-default-org-capture-file nil
  ""
  :type 'file)

;;;###autoload
(defun akirak-url-org-capture (url)
  "Capture URL to the default file."
  (interactive (list (if (equal current-prefix-arg '(16))
                         (akirak-url-complete "Capture URL: ")
                       (or (akirak-url-latest (not current-prefix-arg))
                           (akirak-url-complete "Capture URL: ")))))
  (require 'akirak-org)
  (require 'org-reverse-datetree)
  (unless akirak-url-default-org-capture-file
    (user-error "Variable `akirak-url-default-org-capture-file' is not set"))
  (akirak-org-capture-with-doct
   `(("URL"
      :keys ""
      :template
      (lambda ()
        (let ((heading (org-link-make-string
                        ,url (orgabilize-document-title ,url))))
          (akirak-org-capture-make-entry-body heading :body t)))
      :children
      (("Default file"
        :keys "c"
        :file ,akirak-url-default-org-capture-file
        :function org-reverse-datetree-goto-date-in-file)
       ("Clock"
        :keys "@"
        :clock t
        :contexts ((:function org-clocking-p)))
       ("Datetree file"
        :keys "d"
        :file (lambda ()
                (completing-read "Capture to a datetree: "
                                 (org-dog-file-completion
                                  :class 'org-dog-datetree-file)))
        :function org-reverse-datetree-goto-date-in-file)
       ("Backlog"
        :keys "b"
        :file (lambda ()
                (completing-read "Capture to a backlog: "
                                 (org-dog-file-completion
                                  :class 'org-dog-facade-datetree-file)))
        :function (lambda ()
                    (widen)
                    (goto-char (point-min))
                    (akirak-org-goto-or-create-olp '("Backlog")))))))))

(defun akirak-url-latest (&optional do-not-move)
  "Pick the latest URL from the clipboard."
  (or (when-let (str (akirak-url--match-html-string
                      (current-kill 0 do-not-move)))
        (akirak-url--clean str))
      (car (akirak-url--recent-kills 1))))

(provide 'akirak-url)
;;; akirak-url.el ends here
