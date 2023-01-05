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

(defvar akirak-capture-initial nil)

;;;; Clock

(defun akirak-capture--clock-description ()
  (with-current-buffer (marker-buffer org-clock-marker)
    (org-with-wide-buffer
     (goto-char org-clock-marker)
     (let ((olp (org-get-outline-path t)))
       (format "Clock: \"%s\" in %s (%s)"
               (car (last olp))
               (buffer-name)
               (substring-no-properties
                (org-format-outline-path (butlast olp))))))))

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

(defvar akirak-capture-include-url-fragment nil)

(transient-define-infix akirak-capture-url-fragment ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-capture-include-url-fragment
  :if (lambda () (string-match-p (rx "#") akirak-capture-current-url))
  :description "Url fragment")

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

(transient-define-infix akirak-capture-scheduled-infix ()
  :class 'akirak-capture-plist-option
  :description "Scheduled"
  :variable 'akirak-capture-template-options
  :prop :scheduled
  :reader (lambda (_prompt initial _history)
            (format-time-string (org-time-stamp-format)
                                (org-read-date nil t nil nil nil nil
                                               initial))))

(transient-define-infix akirak-capture-deadline-infix ()
  :class 'akirak-capture-plist-option
  :description "Deadline"
  :variable 'akirak-capture-template-options
  :prop :deadline
  :reader (lambda (_prompt initial _history)
            (format-time-string (org-time-stamp-format)
                                (org-read-date nil t nil nil nil nil
                                               initial))))

(transient-define-infix akirak-capture-doct-add-annotation ()
  :class 'akirak-capture-doct-boolean-option
  :description "Annotation"
  :variable 'akirak-capture-template-options
  :prop :annotation)

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
  :description "Select a parent heading"
  :variable 'akirak-capture-select-heading)

(defun akirak-capture--goto-some-heading ()
  (goto-char (org-ql-completing-read (org-base-buffer (current-buffer))
               :prompt "Select a heading: ")))

(transient-define-prefix akirak-capture-doct ()
  ["Headline and target parent"
   :class transient-row
   ("-h" akirak-capture-headline-infix)
   ("=" akirak-capture-select-heading)]
  ["Entry options"
   :class transient-row
   ("-t" akirak-capture-todo-infix)
   ("-g" akirak-capture-tags-infix)
   ("-s" akirak-capture-scheduled-infix)
   ("-d" akirak-capture-deadline-infix)
   ("-i" akirak-capture-doct-clock-in)
   ("-r" akirak-capture-doct-clock-resume)
   ("-a" akirak-capture-doct-add-annotation)]
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
   ("$" octopus-last-capture-marker-suffix)
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
(transient-define-prefix akirak-capture (&optional initial)
  "Main entry point to capture commands."
  ["Actions (generic / specific type)"
   :class transient-row
   ("T" "Start todo" (lambda ()
                       (interactive)
                       (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                             akirak-capture-template-options '(:todo "UNDERWAY")
                             akirak-capture-doct-options '(:clock-in t :clock-resume t))
                       (akirak-capture-doct))
    :transient t)
   ("t" "Todo" (lambda ()
                 (interactive)
                 (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                       akirak-capture-template-options '(:todo "TODO")
                       akirak-capture-doct-options nil)
                 (akirak-capture-doct))
    :transient t)
   ("#" "Ticket" akirak-capture-ticket
    :transient t)
   ("q" "Question" (lambda ()
                     (interactive)
                     (setq akirak-capture-headline (akirak-capture--maybe-read-heading
                                                    "Question: ")
                           akirak-capture-template-options nil
                           akirak-capture-doct-options '(:clock-in t :clock-resume t))
                     (akirak-capture-doct))
    :transient t)
   ("!" "Troubleshooting" (lambda ()
                            (interactive)
                            (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                                  akirak-capture-template-options (list :todo "UNDERWAY"
                                                                        :tags "@troubleshooting")
                                  akirak-capture-doct-options '(:clock-in t :clock-resume t))
                            (akirak-capture-doct))
    :transient t)
   ("i" "Ideate" (lambda ()
                   (interactive)
                   (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                         akirak-capture-template-options '(:todo "IDEATE" :tags "@ideate")
                         akirak-capture-doct-options '(:clock-in t :clock-resume t))
                   (akirak-capture-doct))
    :transient t)
   ("j" "Journal" akirak-capture-journal)
   ("E" "Epic" (lambda ()
                 (interactive)
                 (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                       akirak-capture-template-options '(:todo "EPIC" :tags "@epic")
                       akirak-capture-doct-options '(:clock-in t :clock-resume t))
                 (akirak-capture-doct))
    :transient t)
   ("l" "With link" (lambda ()
                      (interactive)
                      (setq akirak-capture-headline "%a"
                            akirak-capture-template-options '(:todo "UNDERWAY")
                            akirak-capture-doct-options '(:clock-in t :clock-resume t))
                      (akirak-capture-doct))
    :transient t)
   ("m" "Message" (lambda ()
                    (interactive)
                    (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                          akirak-capture-template-options '(:todo "UNDERWAY" :tags "@message")
                          akirak-capture-doct-options '(:clock-in t :clock-resume t))
                    (akirak-capture-doct))
    :transient t)
   ("r" "Rule" (lambda ()
                 (interactive)
                 (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                       akirak-capture-template-options nil
                       akirak-capture-doct-options '(:function akirak-capture--goto-rules
                                                               :clock-in t :clock-resume t))
                 (akirak-capture-doct))
    :transient t)]

  ["Information (input, events, etc.)"
   :class transient-subgroups
   ["Input"
    :class transient-row
    ("h" "Plain heading"
     (lambda ()
       (interactive)
       (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
             akirak-capture-template-options nil
             akirak-capture-doct-options nil)
       (akirak-capture-doct))
     :transient t)
    ("u" "Url" akirak-capture-url
     :if (lambda () (not akirak-capture-initial)))
    ("v" "Vocabulary" akirak-capture-vocabulary
     :transient t)]

   ["Schedule an event / org-memento"
    :class transient-row
    ("an" "Note"
     (lambda ()
       (interactive)
       (akirak-capture-short-note
        (akirak-capture--maybe-read-heading "Add an event or note: ")))
     :transient t)
    ("aa" "Schedule block"
     (lambda ()
       (interactive)
       (org-memento-add-quick-event
        (akirak-capture--maybe-read-heading "Describe an event: "))))
    ("a!" "Quick start"
     (lambda ()
       (interactive)
       (org-memento-start-quick-event
        (akirak-capture--maybe-read-heading "Describe the current event: "))))

    ("am" "Meeting w/ someone"
     (lambda ()
       (interactive)
       (setq akirak-capture-template-options
             '(:tags "@meeting"
                     :body ("- Participants :: %^{Participants}"
                            ""
                            "%?")))
       (akirak-capture-appointment))
     :transient t)
    ("as" "Session"
     (lambda ()
       (interactive)
       (setq akirak-capture-template-options
             '(:tags "@session"
                     :body ("%?")))
       (akirak-capture-appointment))
     :transient t)
    ("ae" "Errand" akirak-capture-errand
     :transient t)]]

  ["Convenience and specific projects"
   :class transient-row
   ("sc" "Command snippet" akirak-capture-command-snippet
    :transient t)
   ("ss" "Tempo snippet" akirak-capture-simple-tempo-snippet)
   ("e" "Emacs config" akirak-emacs-config-capture)
   ("L" "Journal" akirak-capture-journal-item
    :if (lambda () (eq major-mode 'org-mode)))
   ("P" "Placeholder"
    (lambda ()
      (interactive)
      (org-placeholder-capture-input
       (akirak-capture--maybe-read-heading))))]

  (interactive)
  (cond
   ((equal current-prefix-arg '(16))
    (org-capture-goto-last-stored))
   ((use-region-p)
    (akirak-capture-active-region))
   (t
    (setq akirak-capture-initial initial)
    (when akirak-capture-initial
      (message "Heading set to \"%s\"" akirak-capture-initial))
    (transient-setup 'akirak-capture))))

(defun akirak-capture--maybe-read-heading (&optional prompt)
  (or akirak-capture-initial
      (akirak-capture-read-string (or prompt "Heading: "))))

(transient-define-prefix akirak-capture-active-region ()
  ["Snippet"
   :class transient-row
   ("r" "Tempo snippet" akirak-capture-tempo-snippet)
   ("p" "Plain snippet" akirak-capture-plain-snippet)]
  ["New entry with a block"
   :class
   transient-row
   ("s" "Source"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (ignore-errors
                                          (save-excursion
                                            (goto-char (region-beginning))
                                            (which-function)))
                              :type "src"
                              :clock-in t :clock-resume t))
    :transient t)
   ("S" "Source (no clock-in)"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (ignore-errors
                                          (save-excursion
                                            (goto-char (region-beginning))
                                            (which-function)))
                              :type "src"))
    :transient t)
   ("q" "Quote (with link)"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (akirak-capture-read-string "Headline: ")
                              :annotation t
                              :type "quote"))
    :transient t)
   ("n" "Other"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (akirak-capture-read-string "Headline: ")
                              :clock-in t :clock-resume t))
    :transient t)
   ("N" "Other (immediate finish)"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (akirak-capture-read-string "Headline: ")
                              :immediate-finish t))
    :transient t)

   ("t" "Troubleshooting"
    (lambda ()
      (interactive)
      (akirak-capture--region :headline (akirak-capture--read-summary-for-region
                                         "Headline: ")
                              :todo "UNDERWAY"
                              :tags '("@troubleshooting")
                              :type "example"
                              :clock-in t :clock-resume t))
    :transient t)
   ("l" "Language study" akirak-capture-language-study)]
  ["Others"
   ("a" "Append to clock" akirak-capture-append-block-to-clock
    :if org-clocking-p)]

  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (transient-setup 'akirak-capture-active-region))

(defun akirak-capture-tempo-snippet ()
  (interactive)
  (setq akirak-capture-snippet-format "tempo")
  (setq akirak-capture-snippet-literal-name nil)
  (call-interactively #'akirak-capture-snippet))

(defun akirak-capture-plain-snippet ()
  (interactive)
  (setq akirak-capture-snippet-format "plain")
  (setq akirak-capture-snippet-literal-name nil)
  (call-interactively #'akirak-capture-snippet))

(defun akirak-capture-language-study ()
  "Capture the region for language study."
  (interactive)
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :file ,(akirak-capture--vocabulary-file)
                  :function akirak-capture--goto-backlog
                  :template ,(akirak-org-capture-make-entry-body
                               "%l"
                               :tags '("@input")
                               :body (akirak-capture--org-block "quote"))
                  :immediate-finish t))))))
    (org-capture)))

(cl-defun akirak-capture--region (&rest doct-options
                                        &key type headline tags todo annotation
                                        &allow-other-keys)
  (setq akirak-capture-doct-options (thread-first
                                      doct-options
                                      (plist-put :tags nil)
                                      (plist-put :type nil)
                                      (plist-put :todo nil)
                                      (plist-put :annotation nil)
                                      (plist-put :headline nil)))
  (setq akirak-capture-template-options
        (list :body (concat "%?\n\n" (akirak-capture--org-block type))
              :todo todo
              :annotation annotation
              :tags tags))
  (setq akirak-capture-headline (or headline
                                    (akirak-capture-read-string "Headline: ")))
  (akirak-capture-doct))

(defun akirak-capture--read-summary-for-region (prompt)
  (completing-read prompt
                   (thread-last
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                     (akirak-misc-sentence-lines)
                     (mapcar (lambda (s)
                               (string-trim-right s (rx (+ punct))))))))

(transient-define-prefix akirak-capture-snippet (begin end)
  ["Options"
   :class transient-row
   ("-t" akirak-capture-snippet-format)
   ("-l" akirak-capture-snippet-literal-name)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Other locations"
   :class transient-row
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive "r")
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

(defun akirak-capture-simple-tempo-snippet ()
  (interactive)
  (require 'akirak-snippet)
  (let* ((src (minibuffer-with-setup-hook
                  (lambda ()
                    (lisp-data-mode))
                (read-from-minibuffer "Tempo snippet: ")))
         (org-capture-initial (read-from-minibuffer "Name: "))
         (org-capture-entry
          (car (doct `((""
                        :keys ""
                        :file ,(buffer-file-name)
                        :function ,akirak-snippet-capture-target
                        :template
                        ("* %i :@snippet:"
                         ,akirak-org-capture-default-drawer
                         "%?"
                         ,(thread-last
                            (org-ml-build-src-block
                             :language "lisp-data"
                             :parameters '(:snippet tempo)
                             :value src)
                            (org-ml-to-trimmed-string)))
                        :after-finalize akirak-snippet--after-capture-finalize))))))
    (org-capture)))

(defun akirak-capture-append-block-to-clock ()
  (interactive)
  (require 'akirak-org-clock)
  (akirak-org-clock-require-clock
    (let ((block-text (akirak-capture--org-block))
          (buffer (akirak-org-clock-open)))
      (with-current-buffer buffer
        (goto-char (org-entry-end-position))
        (delete-blank-lines)
        (save-excursion
          (newline 2)
          (insert block-text))
        (newline)))))

;;;###autoload (autoload 'akirak-capture-url "akirak-capture" nil 'interactive)
(transient-define-prefix akirak-capture-url (url)
  [:class
   transient-row
   ("SPC" akirak-capture-source-url)
   ("-f" akirak-capture-url-fragment)]
  ["Options"
   :class transient-row
   ("-t" akirak-capture-todo-infix)
   ("-g" akirak-capture-tags-infix)
   ("-i" akirak-capture-doct-clock-in)
   ("-r" akirak-capture-doct-clock-resume)
   ("=" akirak-capture-select-heading)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Other locations"
   :class transient-row
   ("n" "News"
    (lambda ()
      (interactive)
      (setq akirak-capture-doct-options '(:clock-in t :clock-resume t))
      (octopus--dispatch (octopus-current-command)
                         (akirak-capture--datetree-marker
                          (org-dog-resolve-relative-file "news.org")))))
   ("q" "News queue"
    (lambda ()
      (interactive)
      (let* ((url akirak-capture-current-url)
             (heading (org-link-make-string url (orgabilize-document-title url)))
             (jump-func #'akirak-capture--goto-backlog)
             (org-capture-entry
              (car (doct
                    `((""
                       :keys ""
                       :template ,(akirak-org-capture-make-entry-body
                                    heading :body t)
                       :file ,(org-dog-resolve-relative-file "news.org")
                       :function ,jump-func))))))
        (org-capture))))
   ("@" "Clock"
    (lambda ()
      (interactive)
      (setq akirak-capture-doct-options '(:clock-in t :clock-resume t))
      (octopus--dispatch (octopus-current-command) org-clock-marker)))
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
  (require 'orgabilize)
  (let* ((url akirak-capture-current-url)
         (heading (orgabilize-make-link-string url akirak-capture-include-url-fragment))
         (org-capture-entry
          (car (doct
                `(("Url"
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     heading :body t
                                     akirak-capture-template-options)
                   ,@akirak-capture-doct-options
                   ,@(akirak-capture--target-plist target)))))))
    (org-capture)))

(transient-define-prefix akirak-capture-journal ()
  ["Options"
   ("-a" akirak-capture-doct-add-annotation)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Static files"
   :class transient-row
   :setup-children octopus-setup-static-targets]
  ["Other locations"
   :class transient-row
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (setq akirak-capture-template-options nil)
  (transient-setup 'akirak-capture-journal))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-journal))
                                 target)
  (let* ((file (cl-etypecase target
                 (string target)
                 (org-dog-file (oref target absolute))))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     (akirak-capture--maybe-read-heading)
                                     :body "%?"
                                     akirak-capture-template-options)
                   :file ,file
                   :function org-reverse-datetree-goto-date-in-file
                   :clock-in t :clock-resume t))))))
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
  (let* ((title (akirak-capture-read-string "Appointment title: "))
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

(defun akirak-capture-short-note (string)
  "Add a short note to the journal quickly."
  (interactive "s")
  (let ((org-capture-entry `("" ""
                             entry (file+olp ,org-memento-file
                                             ,(org-memento--today-string))
                             ,(akirak-org-capture-make-entry-body string)
                             :immediate-finish t)))
    (org-capture)))

(defun akirak-capture-vocabulary ()
  (interactive)
  (let* ((file (akirak-capture--vocabulary-file))
         (text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (thing-at-point 'word 'no-properties)))
         (text (or (akirak-capture--find-dictionary-word text)
                   text))
         (derivations (when text
                        (akirak-wordnet-word-derivations text)))
         (in-org-entry (and (derived-mode-p 'org-mode)
                            (not (org-before-first-heading-p)))))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (if (string-match-p org-link-bracket-re candidate)
                 "Link"
               "Derivations")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'word)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred))))
      (let ((input (completing-read "Word or phrase: "
                                    (append (when in-org-entry
                                              (akirak-capture--vocabulary-backlinks))
                                            (cl-remove-duplicates
                                             (cons text derivations)
                                             :test #'equal))
                                    nil nil nil
                                    (if (and (= 1 (length derivations))
                                             (< (length (car derivations))
                                                (length text)))
                                        (car derivations)
                                      text))))
        (if (string-match-p org-link-bracket-re input)
            (org-link-open-from-string input)
          (if-let (marker (akirak-capture--find-heading file input))
              (org-goto-marker-or-bmk marker)
            ;; Depends on an experimental feature of org-super-links.
            (let* ((org-super-links-backlink-into-drawer "VOCABULARY")
                   (sentence-example (org-in-block-p '("quote" "verse" "example")))
                   (selection (if sentence-example
                                  (thing-at-point 'sentence t)
                                (thing-at-point 'paragraph t)))
                   (body (if sentence-example
                             (string-trim selection)
                           (with-temp-buffer
                             (insert (string-trim selection))
                             (goto-char (point-min))
                             (while (re-search-forward (rx (* blank)
                                                           "\n"
                                                           (* blank))
                                                       nil t)
                               (replace-match " "))
                             (buffer-string))))
                   (org-capture-entry
                    (car (doct
                          `((""
                             :keys ""
                             :template ,(akirak-org-capture-make-entry-body
                                          input
                                          :tags (if (string-match-p " " input)
                                                    nil
                                                  '("@word"))
                                          :body (list "#+begin_example"
                                                      body
                                                      "#+end_example"
                                                      "%?"))
                             :file ,file
                             :function akirak-capture--goto-backlog))))))
              (when in-org-entry
                (org-super-links-store-link))
              (org-capture)
              (when in-org-entry
                (org-super-links-insert-link))
              (newline 2)
              (akirak-org-insert-vocabulary-info))))))))

(defun akirak-capture--vocabulary-file ()
  (require 'akirak-org-dog)
  (let ((files (akirak-org-dog-language-files)))
    (or (seq-find (lambda (file)
                    (equal (file-name-base file) "vocabulary"))
                  files)
        (car files))))

(defun akirak-capture--vocabulary-backlinks ()
  (save-excursion
    (org-back-to-heading)
    (catch 'result
      (while (re-search-forward org-drawer-regexp nil t)
        (when (equal (match-string-no-properties 1) "VOCABULARY")
          (let (result
                (bound (save-excursion
                         (re-search-forward org-drawer-regexp nil t))))
            (while (re-search-forward org-link-bracket-re bound t)
              (let ((string (match-string-no-properties 0)))
                (put-text-property 0 (length string)
                                   'display (match-string-no-properties 2)
                                   string)
                (push string result)))
            (throw 'result result)))))))

(defun akirak-capture--find-heading (file heading)
  (with-current-buffer (or (org-find-base-buffer-visiting file)
                           (find-file-noselect file))
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward (format org-complex-heading-regexp-format
                                      (regexp-quote heading))
                              nil t)
       (copy-marker (pos-bol))))))

(defun akirak-capture--find-dictionary-word (word)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "dict/words" (xdg-data-home)))
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (search-forward word nil t)
        (match-string 0)))))

;;;###autoload
(defun akirak-capture-troubleshooting (&optional arg)
  (interactive "P")
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (akirak-capture-read-string "Error message: "))))
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
    (setq akirak-capture-headline (akirak-capture-read-string "Topic: " "#")
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

(defun akirak-capture-journal-item ()
  (interactive)
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :type item
                  :template "%? %a"
                  :function akirak-org-log-goto-week-entry))))))
    (org-capture)))

;;;###autoload
(cl-defun akirak-capture-text (string)
  "Capture a new entry with the selected region as the headline.

This command simply passes STRING to `akirak-capture'. It is
provided as a separate command for integration, e.g. with embark."
  (interactive (list (akirak-capture-read-string "Input: ")))
  (akirak-capture string))

(cl-defun akirak-capture-entry-with-region (&optional body-type &key mode content)
  (interactive)
  (let* ((body-type (or body-type
                        (pcase (org--insert-structure-template-mks)
                          (`("\t" . ,_) (akirak-capture-read-string "Structure type: "))
                          (`(,_ ,choice . ,_) choice))))
         (start-string (concat "#+begin_" body-type
                               (if (equal body-type "src")
                                   (format " %s"
                                           (if (use-region-p)
                                               (string-remove-suffix
                                                "-mode" (symbol-name major-mode))
                                             (completing-read
                                              "Mode: " (akirak-capture--major-mode-list))))
                                 "")))
         (end-string (concat "#+end_" body-type))
         (skeleton `(> "* " ,(format-time-string (org-time-stamp-format t t))
                       n _ n n
                       ,start-string
                       n
                       ,(or content
                            (when (use-region-p)
                              (string-trim (buffer-substring-no-properties
                                            (region-beginning) (region-end))))
                            "")
                       "\n" ,end-string n)))
    (with-current-buffer (generate-new-buffer
                          (generate-new-buffer-name "*org-scratch*"))
      (org-mode)
      (local-set-key (kbd "C-c C-k") #'kill-this-buffer)
      (skeleton-insert skeleton)
      (pop-to-buffer (current-buffer)))))

(cl-defun akirak-capture--org-block (&optional body-type &key mode content)
  (interactive)
  (let* ((body-type (or body-type
                        (pcase (org--insert-structure-template-mks)
                          (`("\t" . ,_) (akirak-capture-read-string "Structure type: "))
                          (`(,_ ,choice . ,_) choice))))
         (start-string (concat "#+begin_" body-type
                               (if (equal body-type "src")
                                   (format " %s"
                                           (if (use-region-p)
                                               (string-remove-suffix
                                                "-mode" (symbol-name major-mode))
                                             (completing-read
                                              "Mode: " (akirak-capture--major-mode-list))))
                                 "")))
         (end-string (concat "#+end_" body-type)))
    (concat start-string "\n"
            (or content
                (when (use-region-p)
                  (let ((region-source (buffer-substring-no-properties
                                        (region-beginning) (region-end))))
                    (if (equal body-type "src")
                        (akirak-capture--unindent region-source)
                      (akirak-capture--to-org region-source))))
                "")
            "\n" end-string "\n")))

(defun akirak-capture--to-org (string)
  ;; `nov-mode' is not a derived mode of `shr-mode'. There is no `shr-mode', so
  ;; you have to check if the current major mode matches any of the known
  ;; shr-based modes.
  (let ((bullet-regexp (when (derived-mode-p 'nov-mode 'eww-mode)
                         (rx-to-string `(and bol (group (*? blank)) ,shr-bullet)))))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (when (looking-at (rx (+ (and (* blank) "\n"))))
        (replace-match ""))
      (when bullet-regexp
        (while (re-search-forward bullet-regexp nil t)
          (replace-match "\\1- ")))
      (buffer-string))))

(defun akirak-capture--unindent (string)
  (let ((lines (split-string string "\n")))
    (cl-flet
        ((indent (s)
           (when (string-match (rx bol (group (+ " ")) (not (any space))) s)
             (- (match-end 1)
                (match-beginning 1)))))
      (let* ((min-indent (thread-last
                           (mapcar #'indent lines)
                           (delq nil)
                           (apply #'min)))
             (regexp (concat "^" (make-string min-indent ?\s))))
        (with-temp-buffer
          (insert string)
          (goto-char (point-min))
          (when (looking-at (rx (+ "\n")))
            (replace-match ""))
          (while (re-search-forward regexp nil t)
            (replace-match ""))
          (buffer-string))))))

(defun akirak-capture--major-mode-list ()
  (let (modes)
    (cl-do-all-symbols (sym)
      (when (and (fboundp sym)
                 (commandp sym)
                 (string-suffix-p "-mode" (symbol-name sym))
                 (not (or (memq sym minor-mode-list)
                          (memq sym global-minor-modes))))
        (push (string-remove-suffix "-mode" (symbol-name sym))
              modes)))
    modes))

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
                      (`("\t" . ,_) (akirak-capture-read-string "Structure type: "))
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

(defun akirak-capture-errand ()
  (interactive)
  (let* ((title (akirak-capture-read-string "Appointment title: "))
         (timestamp (with-temp-buffer
                      (org-time-stamp t)
                      (buffer-string)
                      (goto-char (point-min))
                      (org-element-timestamp-parser)))
         (file (or (oref (org-dog-find-file-object
                          (org-dog-file-pred-1 '(relative "errands.org")))
                         absolute)
                   (error "Failed to locate errands.org")))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(akirak-org-capture-make-entry-body
                                (concat (org-element-property :raw-value timestamp)
                                        " " title)
                                :body
                                (append (when (and (derived-mode-p 'org-mode)
                                                   (not (org-before-first-heading-p)))
                                          '("- %a" ""))
                                        '("| Event | Time | Estimated cost |"
                                          "|-------+------+----------------|"
                                          "| %?    |      |                |"
                                          "|-------+------+----------------|"
                                          "|       |      |                |"
                                          ""
                                          "#+TBLFM: $3=vsum(@2$3..@-1$3)")))
                   :file ,file
                   :function
                   (lambda ()
                     (org-reverse-datetree-goto-date-in-file
                      ',(org-timestamp-to-time timestamp)))))))))
    (org-capture)))

;;;; Helper functions

(defun akirak-capture--goto-backlog ()
  (widen)
  (goto-char (point-min))
  (akirak-org-goto-or-create-olp '("Backlog")))

(defun akirak-capture--goto-rules ()
  (widen)
  (goto-char (point-min))
  (akirak-org-goto-or-create-olp '("Thumb rules")))

(defun akirak-capture--datetree-marker (file)
  "Return a marker to the current date in FILE."
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (org-reverse-datetree-goto-date-in-file nil :return 'marker)))

;;;###autoload
(cl-defun akirak-capture-clock-in (file headline &key tags)
  "Create a new heading with a title and clock into it.

This is intended as the value of `org-dog-clock-in-fallback-fn'."
  (let* ((obj (org-dog-file-object file))
         (jump-func (cond
                     ((object-of-class-p obj 'org-dog-facade-datetree-file)
                      #'akirak-capture--goto-backlog)
                     ((object-of-class-p obj 'org-dog-datetree-file)
                      #'org-reverse-datetree-goto-date-in-file)
                     (t
                      #'akirak-capture--goto-some-heading)))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(akirak-org-capture-make-entry-body
                                headline
                                :todo "UNDERWAY"
                                :tags tags
                                :body "%?")
                   :file ,file
                   :function ,jump-func
                   :clock-in t :clock-resume t))))))
    (org-capture)))

(defun akirak-capture-read-string (prompt &optional initial-contents)
  (minibuffer-with-setup-hook
      (lambda ()
        (abbrev-mode t)
        (corfu-mode t))
    (read-from-minibuffer prompt initial-contents nil nil nil
                          (thing-at-point 'word t)
                          'inherit-input-method)))

(defun akirak-capture-goto-olp-subtree (file &rest olp)
  "Go to an entry inside a subtree."
  (declare (indent 1))
  (let* ((use-cache nil)
         (width (frame-width))
         (candidates (save-current-buffer
                       (org-with-point-at (org-find-olp (cons file olp))
                         (org-map-entries
                          (lambda ()
                            (cons (prog1 (org-format-outline-path
                                          (org-get-outline-path t use-cache)
                                          width nil "/")
                                    (setq use-cache t))
                                  (point-marker)))
                          nil 'tree))))
         (input (completing-read "Parent: " candidates)))
    (if-let (marker (cdr (assoc input candidates)))
        (org-goto-marker-or-bmk marker)
      (find-file file)
      (widen)
      (goto-char (point-min))
      (akirak-org-goto-or-create-olp (split-string input "/")))))

(provide 'akirak-capture)
;;; akirak-capture.el ends here
