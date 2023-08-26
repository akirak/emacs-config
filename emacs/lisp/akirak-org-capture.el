;;; akirak-org-capture.el --- Basic definitions for org-capture -*- lexical-binding: t -*-

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

;; 

;;; Code:

(require 'org)
(require 'ol)
(require 'org-capture)

(defvar org-capture-templates)
(defvar org-capture-templates-contexts)

(defgroup akirak-org-capture
  nil
  ""
  :group 'akirak
  :group 'org-capture)

(defcustom akirak-org-capture-default-drawer
  ":PROPERTIES:
:CREATED_TIME: %U
:END:
"
  "String appended to the body of `org-capture' by default."
  :type 'string)

;;;###autoload
(cl-defun akirak-org-capture-make-entry-body (headline &key
                                                       todo tags
                                                       scheduled
                                                       deadline
                                                       active-ts
                                                       annotation
                                                       properties
                                                       (log-time t)
                                                       (body t)
                                                       level)
  "Build the template body of a capture template.

HEADLINE is a string put in the headline.

TODO specifies the todo keyword of the entry.

TAGS specifies tags of the entry and can be one of the following
values:

 - nil, which means no tag is set.

 - A string.

 - A list of strings.

 - t, which lets you pick a tag from the current buffer (%^g).

 - `all', which lets you pick a tag from all Org buffers (%^G).

SCHEDULED, DEADLINE, and ACTIVE-TS are appended after the
heading. They can be Org timestamps, timestamp strings, or Emacs
internal time representations.

If DRAWER is given, it is inserted before the entry body. By
default, `akirak-org-capture-default-drawer' is inserted. If you
specify an empty string, it will be empty.

The entire content ends with BODY. It can be one of the following
values:

 - A string literal.

 - t, which means the cursor is moved to the point.

Finally, you can specify LEVEL, but you have to set the type of the to plain."
  (declare (indent 0))
  (cl-flet
      ((format-ts (time long)
         (pcase time
           ;; Org timestamp
           ((pred stringp)
            time)
           (`(timestamp . ,_)
            (org-element-property :raw-value time))
           (_
            (org-format-time-string
             (org-time-stamp-format long)
             time))))
       (make-planning (strings)
         (if strings
             (concat (string-join strings " ") "\n")
           "")))
    (concat (if level
                (concat (make-string level ?\*) " ")
              "* ")
            (if todo
                (concat todo " ")
              "")
            (pcase headline
              ((pred stringp) headline)
              (`(url ,url) (org-link-make-string url (orgabilize-document-title url)))
              (`prompt "%^{headline}")
              (`t "%?")
              (_ (error "Invalid headline value: %s" headline)))
            (pcase tags
              (`nil "")
              ((pred stringp) (format " :%s:" tags))
              ((pred listp) (format " :%s:" (string-join tags ":")))
              (`all " %^G")
              (`t " %^g"))
            "\n"
            (thread-first
              (delq nil (list (when scheduled
                                (concat "SCHEDULED: " (format-ts scheduled nil)))
                              (when deadline
                                (concat "DEADLINE: " (format-ts deadline nil)))))
              (make-planning))
            (if-let (properties (if log-time
                                    (cons '("CREATED_TIME" . "%U")
                                          properties)
                                  properties))

                (concat ":PROPERTIES:\n"
                        (mapconcat (pcase-lambda (`(,key . ,value))
                                     (format ":%s: %s" key value))
                                   properties "\n")
                        "\n:END:\n")
              "")
            (if active-ts
                (concat (format-ts active-ts t) "\n")
              "")
            (pcase body
              (`nil "")
              ((pred stringp) body)
              ((pred listp) (string-join body "\n"))
              (`t (if (or (eq headline t)
                          (equal headline "%?"))
                      ""
                    "%?")))
            (if annotation
                "\n%a"
              ""))))

;;;###autoload
(defun akirak-org-capture-add-templates (templates)
  "Add TEMPLATES to `org-capture-templates' without duplicates."
  (declare (indent 1))
  (prog1 org-capture-templates
    (pcase-dolist (`(,key . ,args) templates)
      (if-let (cell (assoc key org-capture-templates))
          (setcdr cell args)
        (add-to-list 'org-capture-templates
                     (cons key args)
                     t)))))

;;;###autoload
(defun akirak-org-capture-with-doct (declarations)
  "Dispatch `org-capture' with doct DECLARATIONS."
  (let ((orig-contexts org-capture-templates-contexts)
        (org-capture-templates (progn
                                 (setq org-capture-templates-contexts nil)
                                 (doct declarations))))
    (unwind-protect
        (org-capture)
      (setq org-capture-templates-contexts orig-contexts))))

;;;###autoload
(defun akirak-org-capture-dog-link-template ()
  "Return a template body containing a file link as the heading."
  (let* ((heading org-capture-initial)
         (filename (org-dog-complete-file "Select a file (or enter an empty string): "
                                          (downcase heading)))
         (object (org-dog-file-object filename)))
    (akirak-org-capture-make-entry-body
      (if object
          (org-link-make-string
           (org-dog-make-file-link object)
           (let ((titles (thread-last
                           (list heading
                                 (org-dog-with-file-header filename
                                   (org-dog-search-keyword-line "title")))
                           (delq nil)
                           (seq-uniq))))
             (completing-read "Link description: "
                              titles nil nil nil nil (car titles))))
        heading))))

(provide 'akirak-org-capture)
;;; akirak-org-capture.el ends here
