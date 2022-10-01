;;; akirak-capture.el --- My capture workflow -*- lexical-binding: t -*-

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

(require 'transient)
(require 'org-capture)
(require 'ol)
(require 'akirak-org-capture)
(require 'akirak-transient)
(require 'akirak-url)
(require 'octopus)

;;;; Variables

(defvar akirak-capture-bounds nil)

;;;; Clock

(defun akirak-capture--clock-description ()
  (org-with-point-at org-clock-marker
    (let ((olp (org-get-outline-path t)))
      (format "Clock: \"%s\" in %s (%s)"
              (car (last olp))
              (buffer-name)
              (substring-no-properties
               (org-format-outline-path (butlast olp)))))))

(defun akirak-capture--to-clock (type template &rest options)
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :type ,type
                  :clock t
                  :template ,template
                  ,@options))))))
    (org-capture)))

;;;; Snippet

(defvar akirak-capture-snippet-format nil)

(defclass akirak-capture-snippet-format-class (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-capture-snippet-format-class))
  (if (equal (oref obj value) "tempo")
      "plain"
    "tempo"))

(transient-define-infix akirak-capture-snippet-format ()
  :class 'akirak-capture-snippet-format-class
  :choices '("plain" "tempo")
  :variable 'akirak-capture-snippet-format
  :description "Type")

(defvar akirak-capture-snippet-literal-name nil)

(transient-define-infix akirak-capture-snippet-literal-name ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-capture-snippet-literal-name
  :description "Literal name")

;;;; URL

(defvar akirak-capture-current-url nil)

(defclass akirak-capture-url-variable (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-capture-url-variable))
  (require 'akirak-url)
  (akirak-url-complete "URL: " (oref obj value)
                       akirak-url-insert-history))

(cl-defmethod transient-format-value ((obj akirak-capture-url-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize (if (> (length value) 30)
                     (concat (substring value 0 30) "...")
                   value)
                 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(transient-define-infix akirak-capture-source-url ()
  :class 'akirak-capture-url-variable
  :variable 'akirak-capture-current-url
  :description "Url")

;;;; akirak-capture-doct: A generic prefix command

(defvar akirak-capture-headline nil)
(defvar akirak-capture-doct-options nil)
(defvar akirak-capture-template-options nil)

(defclass akirak-capture-plist-option (transient-variable)
  ((variable :initarg :variable)
   (prop :initarg :prop)))

(cl-defmethod transient-init-value ((obj akirak-capture-plist-option))
  (oset obj value (plist-get (eval (oref obj variable)) (oref obj prop))))

(cl-defmethod transient-prompt ((obj akirak-capture-plist-option))
  (format "%s: " (oref obj prop)))

(cl-defmethod transient-infix-set ((obj akirak-capture-plist-option) value)
  (oset obj value value)
  (set (oref obj variable)
       (plist-put (eval (oref obj variable))
                  (oref obj prop)
                  value)))

(cl-defmethod transient-format-value ((obj akirak-capture-plist-option))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize (if value
                     (cl-typecase value
                       (string value)
                       (list (string-join value ","))
                       (otherwise (format "%s" value)))
                   "")
                 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

(defclass akirak-capture-doct-boolean-option (akirak-capture-plist-option)
  ((format :initform " %k %d")))

(cl-defmethod transient-infix-read ((obj akirak-capture-doct-boolean-option))
  (not (oref obj value)))

(cl-defmethod transient-format-description ((obj akirak-capture-doct-boolean-option))
  (propertize (oref obj description)
              'face
              (if (oref obj value)
                  'transient-value
                'transient-inactive-value)))

(transient-define-infix akirak-capture-headline-infix ()
  :class 'akirak-transient-variable
  :description "Headline"
  :variable 'akirak-capture-headline)

(transient-define-infix akirak-capture-todo-infix ()
  :class 'akirak-capture-plist-option
  :description "Todo"
  :variable 'akirak-capture-template-options
  :prop :todo
  :reader (lambda (prompt initial history)
            (completing-read prompt
                             (with-temp-buffer
                               (let ((inhibit-startup-hooks t))
                                 (org-mode)
                                 (flatten-tree org-todo-sets)))
                             nil nil
                             initial history)))

(transient-define-infix akirak-capture-tags-infix ()
  :class 'akirak-capture-plist-option
  :description "Tags"
  :variable 'akirak-capture-template-options
  :prop :tags
  :reader (lambda (prompt initial history)
            (completing-read-multiple prompt
                                      (org-global-tags-completion-table
                                       (org-agenda-files))
                                      nil nil
                                      (org-make-tag-string initial)
                                      history)))

(transient-define-infix akirak-capture-doct-clock-in ()
  :class 'akirak-capture-doct-boolean-option
  :description "Clock-in"
  :variable 'akirak-capture-doct-options
  :prop :clock-in)

(transient-define-infix akirak-capture-doct-clock-resume ()
  :class 'akirak-capture-doct-boolean-option
  :description "Clock-resume"
  :variable 'akirak-capture-doct-options
  :prop :clock-resume)

(defvar akirak-capture-select-heading nil)

(transient-define-infix akirak-capture-select-heading ()
  :class 'akirak-transient-flag-variable
  :description "Select a heading"
  :variable 'akirak-capture-select-heading)

(defun akirak-capture--goto-some-heading ()
  (goto-char (org-ql-completing-read (org-base-buffer (current-buffer))
               :prompt "Select a heading: ")))

(transient-define-prefix akirak-capture-doct ()
  ["Infixes"
   ["Options"
    ("-t" akirak-capture-todo-infix)
    ("-h" akirak-capture-headline-infix)
    ("-g" akirak-capture-tags-infix)
    ("-i" akirak-capture-doct-clock-in)
    ("-r" akirak-capture-doct-clock-resume)]
   ["Location"
    ("=" akirak-capture-select-heading)]]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Static files"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other locations"
   :class transient-row
   ("'" octopus-avy-org-heading-suffix)
   ("@" octopus-clock-marker-suffix)
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (transient-setup 'akirak-capture-doct))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-doct))
                                 target)
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :template ,(apply #'akirak-org-capture-make-entry-body
                                    akirak-capture-headline
                                    akirak-capture-template-options)
                  ,@akirak-capture-doct-options
                  ,@(akirak-capture--target-plist target)))))))
    (org-capture)))

(defun akirak-capture--target-plist (target)
  "Build a doct plist from the transient state."
  (let ((jump-func (if akirak-capture-select-heading
                       #'akirak-capture--goto-some-heading
                     #'akirak-capture--goto-backlog)))
    (cl-etypecase target
      (marker (list :function `(lambda ()
                                 (org-goto-marker-or-bmk ,target)
                                 (org-back-to-heading))))
      (string (list :file target
                    :function jump-func))
      (org-dog-file (list :file (oref target absolute)
                          :function jump-func)))))

;;;; Transient prefixes

;;;###autoload (autoload 'akirak-capture "akirak-capture" nil 'interactive)
(transient-define-prefix akirak-capture ()
  "Main entry point to capture commands."
  [:description
   akirak-capture--clock-description
   :if org-clocking-p
   :class transient-row
   ("L" "Link as item"
    (lambda ()
      (interactive)
      (akirak-capture--to-clock 'item "%A%?")))
   ("U" "Url as item"
    (lambda ()
      (interactive)
      (require 'orgabilize)
      (akirak-capture--to-clock
       'item (concat (orgabilize-make-link-string (akirak-url-latest) t)
                     "%?"))))
   ("i" "Item"
    (lambda ()
      (interactive)
      (akirak-capture--to-clock 'item "%?")))
   ("p" "Paragraph"
    (lambda ()
      (interactive)
      (akirak-capture--to-clock 'plain "%?" :empty-lines-before 1)))]

  ["Actions (generic / specific type)"
   :class transient-row
   ("T" "Start todo" (lambda ()
                       (interactive)
                       (setq akirak-capture-headline (read-string "Heading of the todo: ")
                             akirak-capture-template-options '(:todo "UNDERWAY")
                             akirak-capture-doct-options '(:clock-in t :clock-resume t))
                       (akirak-capture-doct)))
   ("t" "Todo" (lambda ()
                 (interactive)
                 (setq akirak-capture-headline (read-string "Heading of the todo: ")
                       akirak-capture-template-options '(:todo "TODO")
                       akirak-capture-doct-options nil)
                 (akirak-capture-doct)))
   ("#" "Ticket" akirak-capture-ticket)
   ;; ("r" "Research topic" (lambda ()
   ;;                         (interactive)
   ;;                         (setq akirak-capture-headline "%^{Title}"
   ;;                               akirak-capture-template-options
   ;;                               '(:todo "UNDERWAY" :tags "@research")
   ;;                               akirak-capture-doct-options (list :clock-in t
   ;;                                                                 :clock-resume t
   ;;                                                                 :jump-to-captured t))
   ;;                         (akirak-capture-doct)))
   ;; ("m" "Compose a message" (lambda ()
   ;;                            (interactive)
   ;;                            (setq akirak-capture-headline "%^{Title}"
   ;;                                  akirak-capture-template-options '(:todo "UNDERWAY"
   ;;                                                                          :tags "@message")
   ;;                                  akirak-capture-doct-options '(:clock-in t :clock-resume t))
   ;;                            (akirak-capture-doct)))
   ("!" "Troubleshoot" akirak-capture-troubleshooting)]

  ["Information (input, events, etc.)"
   :class transient-subgroups
   ["Input"
    :class transient-row
    ("u" "Url" akirak-capture-url)
    ("h" "Plain heading"
     (lambda ()
       (interactive)
       (setq akirak-capture-headline (read-string "Heading: ")
             akirak-capture-template-options nil
             akirak-capture-doct-options nil)
       (akirak-capture-doct)))
    ("n" "Note w/o heading"
     (lambda ()
       (interactive)
       (setq akirak-capture-headline ""
             akirak-capture-template-options '(:tags "@note")
             akirak-capture-doct-options nil)
       (akirak-capture-doct)))
    ("l" "Link as heading"
     (lambda ()
       (interactive)
       (setq akirak-capture-headline "%A"
             akirak-capture-template-options '(:body "%?")
             akirak-capture-doct-options nil)
       (akirak-capture-doct)))]

   ["Schedule an event"
    :class transient-row
    ("am" "Meeting w/ someone"
     (lambda ()
       (interactive)
       (setq akirak-capture-template-options
             '(:tags "@meeting"
                     :body ("- Participants :: %^{Participants}"
                            ""
                            "%?")))
       (akirak-capture-appointment)))
    ("as" "Session"
     (lambda ()
       (interactive)
       (setq akirak-capture-template-options
             '(:tags "@session"
                     :body ("# Links or details"
                            "%?")))
       (akirak-capture-appointment)))
    ("ae" "Errand (@errand)"
     (lambda ()
       (interactive)
       (setq akirak-capture-template-options
             '(:tags "@errand"
                     :body ("| Time | Destination |"
                            "|------+-------------|"
                            "| %? | |")))
       (akirak-capture-appointment)))]]

  [["Convenience"
    ("sc" "Command snippet" akirak-capture-command-snippet)]
   ["Specific projects"
    :class transient-row
    ("e" "Emacs config" akirak-emacs-config-capture)]]

  (interactive)
  (cond
   ((equal current-prefix-arg '(16))
    (org-capture-goto-last-stored))
   ((use-region-p)
    (akirak-capture-active-region))
   (t
    (transient-setup 'akirak-capture))))

(transient-define-prefix akirak-capture-active-region ()
  [:description
   akirak-capture--clock-description
   :if org-clocking-p
   ("b" "Block" akirak-capture-region-to-clock)]

  ["New entry as"
   ("s" "Snippet" akirak-capture-snippet)
   ;; As a wisdom quote
   ;; As a linguistic example
   ("!" "Troubleshooting (with region as an error message)"
    akirak-capture-troubleshooting)]
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (transient-setup 'akirak-capture-active-region))

(transient-define-prefix akirak-capture-snippet (begin end)
  ["Options"
   ("SPC" akirak-capture-snippet-format)
   ("l" akirak-capture-snippet-literal-name)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  (interactive "r")
  (setq akirak-capture-snippet-format "tempo")
  (when (use-region-p)
    (setq akirak-capture-bounds (cons begin end)))
  (transient-setup 'akirak-capture-snippet))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-snippet))
                                 target)
  (require 'akirak-snippet)
  (let ((bounds (if (use-region-p)
                    (car (region-bounds))
                  (bounds-of-thing-at-point 'defun))))
    (funcall (if akirak-capture-snippet-literal-name
                 #'akirak-snippet-capture-literal
               (pcase akirak-capture-snippet-format
                 ("tempo" #'akirak-snippet-capture-tempo)
                 ("plain" #'akirak-snippet-capture)))
             (car bounds) (cdr bounds)
             (cl-etypecase target
               (org-dog-file (oref target absolute))
               (string target)))))

;;;###autoload (autoload 'akirak-capture-url "akirak-capture" nil 'interactive)
(transient-define-prefix akirak-capture-url (url)
  ["Infixes"
   ["Options"
    ("SPC" akirak-capture-source-url)
    ("-t" akirak-capture-todo-infix)
    ("-g" akirak-capture-tags-infix)
    ("-i" akirak-capture-doct-clock-in)
    ("-r" akirak-capture-doct-clock-resume)]
   ["Location"
    ("=" akirak-capture-select-heading)]]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Other locations"
   :class transient-row
   ("@" octopus-clock-marker-suffix)
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive (list (or (akirak-url-latest)
                         (akirak-url-complete "Capture URL: "))))
  (setq akirak-capture-current-url url
        akirak-capture-doct-options nil
        akirak-capture-template-options nil)
  (transient-setup 'akirak-capture-url))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-url))
                                 target)
  (let* ((url akirak-capture-current-url)
         (heading (org-link-make-string url (orgabilize-document-title url)))
         (org-capture-entry
          (car (doct
                `(("Url"
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     heading :body t
                                     akirak-capture-template-options)
                   ,@(akirak-capture--target-plist target)))))))
    (org-capture)))

(defvar akirak-capture-datetime nil)

(transient-define-prefix akirak-capture-appointment ()
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Other locations"
   :class transient-row
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (let* ((title (read-string "Appointment title: "))
         (timestamp (with-temp-buffer
                      (org-time-stamp nil)
                      (goto-char (point-min))
                      (org-element-timestamp-parser))))
    ;; `akirak-capture-template-options' should be set from outside.
    (setq akirak-capture-headline (concat (org-element-property
                                           :raw-value timestamp)
                                          " "
                                          (if (string-empty-p title)
                                              "%^{title}"
                                            title))
          akirak-capture-datetime (org-timestamp-to-time timestamp)))
  (transient-setup 'akirak-capture-appointment))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-appointment))
                                 target)
  (let* ((file (cl-etypecase target
                 (string target)
                 (org-dog-file (oref target absolute))))
         (datetreep (cl-typecase (org-dog-file-object file)
                      (org-dog-datetree-file t)
                      (otherwise nil)))
         (plist (when datetreep
                  (list :function
                        `(lambda ()
                           (org-reverse-datetree-goto-date-in-file
                            ',akirak-capture-datetime)))))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     akirak-capture-headline
                                     akirak-capture-template-options)
                   :file ,file
                   ,@plist))))))
    (org-capture)))


;;;; Other commands

;;;###autoload
(defun akirak-capture-troubleshooting (&optional arg)
  (interactive "P")
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string "Error message: "))))
    (when (string-empty-p (string-trim text))
      (setq text nil))
    (setq akirak-capture-headline (if (and text
                                           (string-match (rx bos (* space) (group (+ nonl)))
                                                         text))
                                      (match-string 1 text)
                                    "%?")
          akirak-capture-doct-options '(:clock-in t :clock-resume t)
          akirak-capture-template-options `(:todo "UNDERWAY"
                                                  :tags "@troubleshooting"
                                                  :body
                                                  ,(when text
                                                     (list "%?"
                                                           "#+begin_example"
                                                           text
                                                           "#+end_example"))))
    (akirak-capture-doct)))

(defun akirak-capture-ticket ()
  (interactive)
  (if (derived-mode-p 'forge-topic-mode)
      (let ((plist (akirak-capture--forge-topic)))
        (setq akirak-capture-headline (concat forge-buffer-topic-ident " "
                                              (plist-get plist :title))
              akirak-capture-template-options `(:todo "TODO"
                                                      :body
                                                      (list "%?"
                                                            "#+begin_quote"
                                                            (plist-get plist :content)
                                                            "#+end_quote"))))
    (setq akirak-capture-headline (read-string "Topic: " "#")
          akirak-capture-template-options `(:todo "TODO")))
  (setq akirak-capture-doct-options '(:clock-in t :clock-resume t))
  (akirak-capture-doct))

(defun akirak-capture--forge-topic ()
  "Parse the content of a forge topic buffer.

This is a hack that parses the buffer of a special mode. It may
not work in the future when forge changes the output."
  (save-excursion
    (cl-labels
        ((topic-property (key)
           (goto-char (point-min))
           (re-search-forward (concat key ":[[:blank:]]+"))
           (let ((string (buffer-substring-no-properties (point) (pos-eol))))
             (unless (equal string "none")
               string))))
      (list :title (topic-property "Title")
            ;; :labels (topic-property "Labels")
            ;; :milestone (topic-property "Milestone")
            :content (progn
                       (goto-char (point-min))
                       (magit-section-forward-sibling)
                       (let ((start (point)))
                         (magit-section-forward-sibling)
                         (buffer-substring-no-properties start (point))))))))

;;;###autoload
(cl-defun akirak-capture-text (string &key as-body)
  "Capture a new entry with the selected region as the headline."
  (interactive (if (equal current-prefix-arg '(4))
                   (list (read-string "Content: ")
                         :as-body t)
                 (list (read-string "Headline: "))))
  (if as-body
      (let ((body-type (pcase (org--insert-structure-template-mks)
                         (`("\t" . ,_) (read-string "Structure type: "))
                         (`(,_ ,choice . ,_) choice))))
        (setq akirak-capture-headline "%^{Title}"
              akirak-capture-template-options
              `(:body ,(list "%?"
                             (concat "#+begin_" body-type
                                     (when (equal body-type "src")
                                       (thread-last
                                         (symbol-name major-mode)
                                         (string-remove-suffix "-mode")
                                         (concat " "))))
                             string
                             (concat "#+end_" (car (split-string body-type)))
                             "%a"))
              akirak-capture-doct-options nil))
    (setq akirak-capture-headline string
          akirak-capture-template-options nil
          akirak-capture-doct-options nil))
  (akirak-capture-doct))

(defun akirak-capture-command-snippet ()
  (interactive)
  (setq akirak-capture-headline "%^{Title}"
        akirak-capture-template-options
        `(:tags "@snippet"
                :body ("%?"
                       "#+begin_src emacs-lisp :snippet command :no-save-mark t"
                       "#+end_src"))
        akirak-capture-doct-options
        (list :function #'akirak-snippet-select-location
              :after-finalize #'akirak-snippet--after-capture-finalize))
  (akirak-capture-doct))

(defun akirak-capture-region-to-clock ()
  (interactive)
  (akirak-capture--to-clock
   'plain
   (let ((body-type (pcase (org--insert-structure-template-mks)
                      (`("\t" . ,_) (read-string "Structure type: "))
                      (`(,_ ,choice . ,_) choice))))
     (list (concat "#+begin_" body-type
                   (when (equal body-type "src")
                     (thread-last
                       (symbol-name major-mode)
                       (string-remove-suffix "-mode")
                       (concat " "))))
           (replace-regexp-in-string
            "%" "%%" (buffer-substring-no-properties (region-beginning) (region-end)))
           (concat "#+end_" (car (split-string body-type)))))
   :empty-lines-before 1))

;;;; Helper functions

(defun akirak-capture--goto-backlog ()
  (widen)
  (goto-char (point-min))
  (akirak-org-goto-or-create-olp '("Backlog")))

(provide 'akirak-capture)
;;; akirak-capture.el ends here
