;;; akirak-capture.el --- My capture workflow -*- lexical-binding: t -*-

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

;; 

;;; Code:

(require 'transient)
(require 'org-capture)
(require 'ol)
(require 'akirak-org-capture)
(require 'akirak-transient)
(require 'akirak-url)
(require 'octopus)

(defconst akirak-capture-zero-width-characters
  (mapconcat #'char-to-string (list 8203 8288 8205 8204 65279)))

;;;; Variables

(defvar akirak-capture-bounds nil)

(defvar akirak-capture-initial nil)

(defvar akirak-capture-clocked-buffer-info nil)

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

(defvar akirak-capture-url-title nil)

(transient-define-infix akirak-capture-url-title ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-capture-url-title
  :initial-contents-fn 'akirak-capture-initial-url-title
  :prompt "Title: "
  :description "Title")

(defun akirak-capture-initial-url-title ()
  (require 'orgabilize)
  (let* ((url akirak-capture-current-url)
         (clean-url (orgabilize--url-for-link url)))
    (if akirak-capture-include-url-fragment
        (if-let* ((fragment (orgabilize-document-fragment url)))
            (orgabilize-document-fragment-title clean-url fragment)
          (orgabilize-document-title clean-url))
      (orgabilize-document-title clean-url))))

;;;; Clock history

(transient-define-suffix akirak-capture-org-clock-history-suffix ()
  :description "Clock history"
  (interactive)
  (akirak-consult-org-clock-history nil
    :prompt "Capture into: "
    :callback (apply-partially #'octopus--dispatch (octopus-current-command))))

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

(defvar akirak-capture-gptel-topic nil)

(transient-define-infix akirak-capture-gptel-infix ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-capture-gptel-topic
  :description "Gptel topic")

(transient-define-infix akirak-capture-dispatch-later ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-capture-dispatch-later
  :description "Dispatch later")

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
                                      (akirak-capture--org-global-tags)
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

(transient-define-infix akirak-capture-doct-immediate-finish ()
  :class 'akirak-capture-doct-boolean-option
  :description "Immediate finish"
  :variable 'akirak-capture-doct-options
  :prop :immediate-finish)

(defvar akirak-capture-select-heading nil)

(transient-define-infix akirak-capture-select-heading ()
  :class 'akirak-transient-flag-variable
  :description "Select a parent heading"
  :variable 'akirak-capture-select-heading)

(defvar akirak-capture-date nil)

(transient-define-infix akirak-capture-change-date-infix ()
  :class 'akirak-transient-flag-variable
  :description "Select a date"
  :variable 'akirak-capture-date)

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
   ("-e" akirak-capture-gptel-infix)
   ("-t" akirak-capture-todo-infix)
   ("-g" akirak-capture-tags-infix)
   ("-s" akirak-capture-scheduled-infix)
   ("-d" akirak-capture-deadline-infix)
   ("-i" akirak-capture-doct-clock-in)
   ("-r" akirak-capture-doct-clock-resume)
   ("-I" akirak-capture-doct-immediate-finish)
   ("-a" akirak-capture-doct-add-annotation)]
  ["Gptel"
   :class transient-row
   :if-non-nil akirak-capture-gptel-topic
   ("-c" akirak-capture-dispatch-later)
   (gptel--infix-provider)]
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
   ("/" octopus-read-dog-file-suffix)
   ("$" octopus-last-captured-file-suffix)
   ("%" akirak-capture-org-clock-history-suffix)]
  (interactive)
  (transient-setup 'akirak-capture-doct))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-doct))
                                 target)
  (let* ((template-options (if akirak-capture-gptel-topic
                               (thread-last
                                 (plist-get akirak-capture-template-options :properties)
                                 (cons `("GPTEL_TOPIC" . ,(akirak-capture--escape-gptel-topic
                                                           akirak-capture-headline)))
                                 (plist-put akirak-capture-template-options :properties))
                             akirak-capture-template-options))
         ;; When this variable is non-nil, start the capture session in a new
         ;; tab.
         (new-tab-name (when akirak-capture-gptel-topic
                         akirak-capture-headline))
         (doct-options (if akirak-capture-gptel-topic
                           (thread-first
                             akirak-capture-doct-options
                             (plist-put :hook
                                        `(lambda ()
                                           ;; Set some delay for Emacs to initialize the buffer.
                                           (run-with-timer
                                            0.1
                                            nil
                                            (lambda ()
                                              (when ,new-tab-name
                                                (tab-bar-rename-tab ,new-tab-name)
                                                (when (fboundp 'fwb-toggle-window-split)
                                                  (fwb-toggle-window-split)))
                                              (when (and akirak-capture-gptel-topic
                                                         (not akirak-capture-dispatch-later))
                                                (require 'gptel-org)
                                                (gptel-send))))))
                             (plist-put :after-finalize
                                        (when new-tab-name
                                          `(lambda ()
                                             (tab-bar-close-tab-by-name ,new-tab-name)))))
                         akirak-capture-doct-options))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     akirak-capture-headline
                                     template-options)
                   ,@doct-options
                   ,@(akirak-capture--target-plist target)))))))
    ;; This is not an ideal solution. After finishing the capture session, it
    ;; restores the window configuration instead of closing the new tab.
    (when new-tab-name
      (tab-bar-new-tab)
      (delete-other-windows))
    ;; I am not sure if it is necessary, but just in case return the value from
    ;; `org-capture'.
    (prog1 (org-capture)
      (setq akirak-capture-gptel-topic nil))))

(defun akirak-capture--add-property ())

(defun akirak-capture--target-plist (target)
  "Build a doct plist from the transient state."
  (let ((jump-func (if akirak-capture-select-heading
                       #'akirak-capture--goto-some-heading
                     #'akirak-capture--goto-backlog)))
    (cl-etypecase target
      (marker (list :file (buffer-file-name (org-base-buffer (marker-buffer target)))
                    :olp (org-with-point-at target
                           (org-get-outline-path t))))
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
   ;; ("#" "Ticket" akirak-capture-ticket
   ;;  :transient t)
   ("q" "Question" (lambda ()
                     (interactive)
                     (setq akirak-capture-headline (akirak-capture--maybe-read-heading
                                                    "Question: ")
                           akirak-capture-template-options nil
                           akirak-capture-doct-options '(:clock-in t :clock-resume t))
                     (akirak-capture-doct))
    :transient t)
   ("g" "Gptel" akirak-capture-gptel
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
   ;; ("L" "Start todo with link" (lambda ()
   ;;                               (interactive)
   ;;                               (setq akirak-capture-headline (akirak-capture--make-org-link)
   ;;                                     akirak-capture-template-options '(:todo "UNDERWAY")
   ;;                                     akirak-capture-doct-options '(:clock-in t :clock-resume t))
   ;;                               (akirak-capture-doct))
   ;;  :transient t)
   ("l" "Bookmark as link" (lambda ()
                             (interactive)
                             (setq akirak-capture-headline (akirak-capture--make-org-link)
                                   akirak-capture-template-options '(:tags "@bookmark")
                                   akirak-capture-doct-options nil)
                             (akirak-capture-doct))
    :transient t)
   ("F" "Hotfix" akirak-capture-hotfix)
   ("/" "Tag prompt" akirak-capture-entry-with-tag
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
    ("r" "Read url right now"
     (lambda ()
       (interactive)
       (setq akirak-capture-current-url (or (akirak-url-latest)
                                            (akirak-url-complete "Capture URL: "))
             akirak-capture-url-title nil
             akirak-capture-doct-options '(:clock-in t :clock-resume t)
             akirak-capture-template-options '(:todo "UNDERWAY" :tags "@reading"))
       (akirak-capture-url nil 'keep-options))
     :if (lambda () (not akirak-capture-initial)))]

   ;; ["Schedule an event / org-memento"
   ;;  :class transient-row
   ;;  ("an" "Note"
   ;;   (lambda ()
   ;;     (interactive)
   ;;     (akirak-capture-short-note
   ;;      (akirak-capture--maybe-read-heading "Add an event or note: "))))
   ;;  ("aa" "Schedule block"
   ;;   (lambda ()
   ;;     (interactive)
   ;;     (org-memento-add-quick-event
   ;;      (akirak-capture--maybe-read-heading "Describe an event: "))))
   ;;  ("a!" "Quick start"
   ;;   (lambda ()
   ;;     (interactive)
   ;;     (org-memento-start-quick-event
   ;;      (akirak-capture--maybe-read-heading "Describe the current event: "))))

   ;;  ;; ("am" "Meeting w/ someone"
   ;;  ;;  (lambda ()
   ;;  ;;    (interactive)
   ;;  ;;    (setq akirak-capture-template-options
   ;;  ;;          '(:tags "@meeting"
   ;;  ;;                  :body ("- Participants :: %^{Participants}"
   ;;  ;;                         ""
   ;;  ;;                         "%?")))
   ;;  ;;    (akirak-capture-appointment))
   ;;  ;;  :transient t)
   ;;  ;; ("as" "Session"
   ;;  ;;  (lambda ()
   ;;  ;;    (interactive)
   ;;  ;;    (setq akirak-capture-template-options
   ;;  ;;          '(:tags "@session"
   ;;  ;;                  :body ("%?")))
   ;;  ;;    (akirak-capture-appointment))
   ;;  ;;  :transient t)
   ;;  ;; ("ae" "Errand" akirak-capture-errand
   ;;  ;;  :transient t)
   ;;  ]
   ]

  ["Convenience and specific projects"
   :class transient-row
   ;; ("sc" "Command snippet" akirak-capture-command-snippet
   ;;  :transient t)
   ;; ("ss" "Tempo snippet" akirak-capture-simple-tempo-snippet)
   ("e" "Emacs config" akirak-emacs-config-capture)
   ("L" "Journal" akirak-capture-journal-item
    :if (lambda () (eq major-mode 'org-mode)))]

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

(defun akirak-capture--make-org-link ()
  (call-interactively #'org-store-link)
  (let ((link (pop org-stored-links)))
    (org-link-make-string (car link)
                          (read-from-minibuffer "Description: "
                                                (cadr link)))))

(defcustom akirak-capture-tag-alist
  '(("@troubleshooting"
     :template (:todo "UNDERWAY")
     :doct (:clock-in t :clock-resume t))
    ("@message"
     :template (:todo "UNDERWAY")
     :doct (:clock-in t :clock-resume t))
    ("@note"
     :doct (:clock-in t :clock-resume t)))
  ""
  :type '(alist :key-type (string :tag "Org tag")
                :value-type plist))

(defun akirak-capture-entry-with-tag (tag)
  (interactive (list (completing-read "Org tag: "
                                      (akirak-capture--org-global-tags)
                                      nil nil "@")))
  (let ((plist (cdr (assoc tag akirak-capture-tag-alist))))
    (setq akirak-capture-doct-options (plist-get plist :doct)
          akirak-capture-template-options (thread-first
                                            (plist-get plist :template)
                                            (plist-put :tags tag))
          akirak-capture-headline (akirak-capture--maybe-read-heading))
    (akirak-capture-doct)))

(defun akirak-capture--org-global-tags ()
  (thread-last
    (mapcar #'car org-tag-persistent-alist)
    (cl-remove-if-not #'stringp)))

;;;###autoload (autoload 'akirak-capture-active-region "akirak-capture" nil 'interactive)
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

   ;; ("t" "Troubleshooting"
   ;;  (lambda ()
   ;;    (interactive)
   ;;    (akirak-capture--region :headline (akirak-capture--read-summary-for-region
   ;;                                       "Headline: ")
   ;;                            :todo "UNDERWAY"
   ;;                            :tags '("@troubleshooting")
   ;;                            :type "example"
   ;;                            :clock-in t :clock-resume t))
   ;;  :transient t)
   ("l" "Language study (input)" akirak-capture-language-study)
   ("v" "Vocabulary" akirak-capture-vocabulary)
   ("g" "Gptel" akirak-capture-gptel :transient t)]
  ["Others" :class transient-row
   ("b" "Convert to a link to a new entry" akirak-org-convert-to-entry-link)
   ("a" "Append block to clock" akirak-capture-append-block-to-clock
    :if org-clocking-p)
   ("h" "Append heading to clock" akirak-capture-append-heading-to-clock
    :if org-clocking-p)]

  (interactive)
  (if (use-region-p)
      (setq akirak-capture-bounds (car (region-bounds)))
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
                               "%A"
                               :tags '("@input")
                               :body (akirak-capture--org-block "example"))))))))
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

(defun akirak-capture-hotfix ()
  "Start clocking a hotfix activity on the current line."
  (interactive)
  ;; Similar to `akirak-capture-clock-in'.
  (pcase (akirak-org-clock-find-commit-entry)
    ((and (map :marker :summary)
          (guard marker))
     (let ((org-capture-entry
            (car (doct
                  `((""
                     :keys ""
                     :template ,(akirak-org-capture-make-entry-body
                                  (format "Hotfix of %s" summary)
                                  :todo "TODO"
                                  :tags "@hotfix")
                     :function (lambda ()
                                 (org-goto-marker-or-bmk ,marker))
                     :clock-in t :clock-resume t))))))
       (save-window-excursion
         (org-capture))))
    (_
     (user-error "Cannot find an Org entry"))))

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
        (save-excursion
          (beginning-of-line 0)
          (when (looking-at (rx eol))
            (delete-blank-lines)))
        (newline)
        (insert block-text))
      (when-let* ((window (get-buffer-window buffer)))
        (with-selected-window window
          (unless (looking-at org-heading-regexp)
            (goto-char (org-entry-end-position))))))))

(defun akirak-capture--same-level-heading ()
  (interactive)
  (akirak-capture--append-heading-to-clock
   (plist-get akirak-capture-clocked-buffer-info :level)
   akirak-capture-initial))

(defun akirak-capture--subheading ()
  (interactive)
  (akirak-capture--append-heading-to-clock
   (1+ (plist-get akirak-capture-clocked-buffer-info :level))
   akirak-capture-initial))

(defconst akirak-capture-heading-commands
  (let (result)
    (dolist (level (number-sequence 1 9))
      (let ((symbol (intern (format "akirak-capture--heading-level-%d" level))))
        (fset symbol `(lambda ()
                        (interactive)
                        (akirak-capture--append-heading-to-clock
                         ,level akirak-capture-initial)))
        (put symbol 'interactive-only t)
        (push (cons level symbol) result)))
    result))

(defun akirak-capture--heading-capture-children (children)
  (let (result
        (min-level (org-element-property
                    :level (org-element-at-point-no-context org-clock-hd-marker))))
    (dolist (n (number-sequence min-level 9))
      (push (list transient--default-child-level
                  'transient-suffix
                  (list :key (int-to-string n)
                        :description (format "Level %d" n)
                        :command (cdr (assq n akirak-capture-heading-commands))
                        :transient nil))
            result))
    (append children
            (nreverse result))))

(transient-define-prefix akirak-capture-append-heading-to-clock (text)
  [:description
   (lambda ()
     (format "New heading \"%s\"" akirak-capture-initial))
   :class transient-row
   ("-g" akirak-capture-tags-infix)]
  [:description
   (lambda ()
     (format "After heading at level %d: %s"
             (plist-get akirak-capture-clocked-buffer-info :level)
             (plist-get akirak-capture-clocked-buffer-info :title)))
   :class transient-row
   :setup-children akirak-capture--heading-capture-children
   ("=" "Same level" akirak-capture--same-level-heading)
   ("+" "Subheading" akirak-capture--subheading)]
  (interactive (list (thread-last
                       (cond
                        ((use-region-p)
                         (buffer-substring-no-properties begin end))
                        (akirak-capture-bounds
                         (buffer-substring-no-properties
                          (car akirak-capture-bounds)
                          (cdr akirak-capture-bounds)))
                        (t
                         (read-from-minibuffer "Heading: ")))
                       (replace-regexp-in-string (rx (+ space)) " ")
                       (string-trim))))
  (setq akirak-capture-initial text)
  (setq akirak-capture-clocked-buffer-info (akirak-capture--clocked-buffer-info))
  (transient-setup 'akirak-capture-append-heading-to-clock))

(defun akirak-capture--clocked-buffer-info ()
  (akirak-org-clock-require-clock
    (with-current-buffer (akirak-org-clock-open)
      (let ((node (org-element-context)))
        (when-let* ((h (catch 'headline
                         (while node
                           (when (eq (org-element-type node) 'headline)
                             (throw 'headline node))
                           (setq node (org-element-parent node))))))
          (list :level (org-element-property :level h)
                :title (org-element-property :title h)))))))

(defun akirak-capture--append-heading-to-clock (level heading)
  (akirak-org-clock-require-clock
    (let* ((buffer (akirak-org-clock-open))
           (pos (with-current-buffer buffer
                  (goto-char (org-entry-end-position))
                  (unless (bolp)
                    (newline))
                  (insert (make-string level ?\*) " " heading)
                  (point))))
      (when-let* ((window (get-buffer-window buffer)))
        (with-selected-window window
          (goto-char pos))))))

(transient-define-suffix akirak-capture-url-to-clock ()
  :description 'octopus-clocked-entry-description
  :if 'org-clocking-p
  (interactive)
  (setq akirak-capture-doct-options '(:clock-in t :clock-resume t))
  (octopus--dispatch (octopus-current-command) org-clock-marker))

;;;###autoload (autoload 'akirak-capture-url "akirak-capture" nil 'interactive)
(transient-define-prefix akirak-capture-url (url &optional keep-options)
  [:class
   transient-row
   ("SPC" akirak-capture-source-url)
   ("-f" akirak-capture-url-fragment)
   ("C-l" akirak-capture-url-title)]
  ["Options"
   :class transient-row
   ("-t" akirak-capture-todo-infix)
   ("-g" akirak-capture-tags-infix)
   ("-i" akirak-capture-doct-clock-in)
   ("-r" akirak-capture-doct-clock-resume)
   ("-I" akirak-capture-doct-immediate-finish)
   ("=" akirak-capture-select-heading)]
  ["Context"
   :class transient-columns
   :setup-children octopus-setup-context-file-subgroups]
  ["Static files"
   :class transient-row
   :setup-children octopus-setup-static-targets]
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
             (heading (org-link-make-string url (or akirak-capture-url-title
                                                    (orgabilize-document-title url))))
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
   ("'" octopus-avy-org-heading-suffix)
   ("@" akirak-capture-url-to-clock)
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)
   ("$" octopus-last-captured-file-suffix)]
  (interactive (list (or (akirak-url-latest)
                         (akirak-url-complete "Capture URL: "))))
  (unless keep-options
    (setq akirak-capture-current-url url
          akirak-capture-url-title nil
          akirak-capture-doct-options nil
          akirak-capture-template-options nil))
  (transient-setup 'akirak-capture-url))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-url))
                                 target)
  (require 'orgabilize)
  (let* ((url akirak-capture-current-url)
         (heading (if akirak-capture-url-title
                      (org-link-make-string url akirak-capture-url-title)
                    (orgabilize-make-link-string url akirak-capture-include-url-fragment)))
         (org-capture-entry
          (car (doct
                `(("Url"
                   :keys ""
                   :template ,(apply #'akirak-org-capture-make-entry-body
                                     heading :body t
                                     akirak-capture-template-options)
                   ,@akirak-capture-doct-options
                   ,@(akirak-capture--target-plist target))))))
         ;; If the current buffer is `org-mode', override `display-buffer-alist'
         ;; to display the capture buffer in the same window.
         (display-buffer-alist (if (eq major-mode 'org-mode)
                                   '(("^CAPTURE-"
                                      display-buffer-same-window
                                      (inhibit-same-window . nil)))
                                 display-buffer-alist)))
    (org-capture)))

(transient-define-prefix akirak-capture-journal ()
  ["Options"
   :class transient-row
   ("-a" akirak-capture-doct-add-annotation)
   ("-j" akirak-capture-change-date-infix)]
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
  (setq akirak-capture-template-options nil
        akirak-capture-date nil)
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
                   :function ,(if akirak-capture-date
                                  #'org-reverse-datetree-goto-read-date-in-file
                                #'org-reverse-datetree-goto-date-in-file)
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

;;;###autoload
(defun akirak-capture-gptel (headline llm-prompt)
  (interactive
   (let* ((llm-prompt (unless current-prefix-arg
                        (read-string "Prompt: "
                                     (when (use-region-p)
                                       (buffer-substring (region-beginning)
                                                         (region-end)))
                                     nil nil 'inherit)))
          (headline (read-string "Headline: "
                                 (when llm-prompt
                                   (akirak-capture--first-sentence llm-prompt))
                                 nil nil 'inherit)))
     (list headline llm-prompt)))
  (require 'gptel)
  (cl-flet
      ((file-link (filename)
         (thread-last
           (concat "file:" (abbreviate-file-name filename))
           (org-link-make-string))))
    ;; NOTE: This depends on the private API of gptel.
    (let ((dispatch-later (string-match-p (rx "%?") llm-prompt))
          (preamble (pcase gptel-context--alist
                      (`nil)
                      (`((,buffer . ,ovs))
                       (concat (when ovs
                                 (with-current-buffer buffer
                                   (mapconcat (lambda (ov)
                                                ;; TODO: Add the language for the mode
                                                (concat "#+begin_src\n"
                                                        (string-trim-right
                                                         (buffer-substring (overlay-start ov)
                                                                           (overlay-end ov)))
                                                        "\n#+end_src\n\n"))
                                              ovs
                                              "")))
                               (when-let* ((filename (buffer-file-name
                                                      (or (buffer-base-buffer buffer)
                                                          buffer))))
                                 (concat (file-link filename) "\n\n"))))
                      (files
                       (concat (mapconcat (lambda (filename)
                                            (concat "- " (file-link filename)))
                                          files
                                          "\n")
                               "\n\n")))))
      (setq akirak-capture-gptel-topic t
            akirak-capture-dispatch-later dispatch-later
            akirak-capture-headline headline
            akirak-capture-template-options (list :tags "@AI"
                                                  :body
                                                  (concat preamble llm-prompt
                                                          (unless dispatch-later
                                                            "%?"))))
      (akirak-capture-doct))))

(defun akirak-capture-short-note (string)
  "Add a short note to the journal quickly."
  (interactive "s")
  (let ((org-capture-entry `("" ""
                             entry (file+olp ,org-memento-file
                                             ,(org-memento--today-string))
                             ,(akirak-org-capture-make-entry-body string)
                             :immediate-finish t)))
    (org-capture)))

;; Currently not used.
(defun akirak-capture-check-item-to-clock ()
  (interactive)
  ;; Don't use %A. I want to keep the window configuration while typing the
  ;; description.
  (let ((org-capture-entry `("" "" checkitem (clock))))
    (org-capture)))

;; Currently not used.
(defun akirak-capture-link-item-to-clock ()
  (interactive)
  ;; Don't use %A. I want to keep the window configuration while typing the
  ;; description.
  (let* ((inhibit-message t)
         (link (progn
                 (org-store-link nil t)
                 (pop org-stored-links)))
         (link-string (org-link-make-string
                       (car link)
                       (if (derived-mode-p 'org-mode)
                           (cadr link)
                         (read-from-minibuffer "Description: "
                                               nil nil nil nil
                                               (cadr link)))))
         (org-capture-entry `("" "" item (clock)
                              ,(concat link-string "%?"))))
    (org-capture)))

;; Currently not used.
(defun akirak-capture-vocabulary ()
  (interactive)
  (let* ((file (akirak-capture--vocabulary-file))
         (text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (thing-at-point 'word 'no-properties)))
         (text (or (when text
                     (akirak-capture--find-dictionary-word text))
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
          (if-let* ((marker (akirak-capture--find-heading file input)))
              (org-goto-marker-or-bmk marker)
            ;; Depends on an experimental feature of org-super-links.
            (let* ((org-super-links-backlink-into-drawer "VOCABULARY")
                   (sentence-example (org-in-block-p '("quote" "verse" "example")))
                   (selection (org-link-display-format
                               (if sentence-example
                                   (thing-at-point 'sentence t)
                                 (thing-at-point 'paragraph t))))
                   (body (cond
                          (sentence-example
                           (string-trim selection))
                          (selection
                           (with-temp-buffer
                             (insert (string-trim selection))
                             (goto-char (point-min))
                             (while (re-search-forward (rx (* blank)
                                                           "\n"
                                                           (* blank))
                                                       nil t)
                               (replace-match " "))
                             (buffer-string)))))
                   (org-capture-entry
                    (car (doct
                          `((""
                             :keys ""
                             :template ,(akirak-org-capture-make-entry-body
                                          input
                                          :tags (if (string-match-p " " input)
                                                    nil
                                                  '("@word"))
                                          :body (append (when body
                                                          (list "#+begin_example"
                                                                body
                                                                "#+end_example"
                                                                "%?"))))
                             :file ,file
                             :function akirak-capture--goto-backlog))))))
              (if (and body in-org-entry)
                  (progn
                    (org-super-links-store-link)
                    (org-capture)
                    (org-super-links-insert-link)
                    (newline 2))
                (org-capture)))))))))

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
                                               (thread-last
                                                 (symbol-name major-mode)
                                                 (string-remove-suffix "-mode")
                                                 (string-remove-suffix "-ts"))
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
                                           (if (and (use-region-p)
                                                    (not (derived-mode-p 'special-mode)))
                                               (akirak-org--find-src-lang
                                                (thread-last
                                                  (symbol-name major-mode)
                                                  (string-remove-suffix "-mode")
                                                  (string-remove-suffix "-ts")))
                                             (completing-read
                                              "Mode: " (akirak-capture--major-mode-list))))
                                 "")))
         (end-string (concat "#+end_" body-type)))
    (concat start-string "\n"
            (or content
                (when-let* ((region-source (akirak-capture--region-text)))
                  (if (equal body-type "quote")
                      (akirak-capture--to-org
                       (progn
                         (require 'akirak-pandoc)
                         (if-let* ((format (akirak-pandoc-input-format)))
                             (akirak-pandoc-convert-string region-source
                               :from format :to "org")
                           region-source)))
                    ;; Newlines are significant in most of the block types, so
                    ;; use the source sanitizer for now.
                    (akirak-capture--sanitize-source region-source)))
                "")
            "\n" end-string "\n")))

(defun akirak-capture--to-org (string)
  ;; `nov-mode' is not a derived mode of `shr-mode'. There is no `shr-mode', so
  ;; you have to check if the current major mode matches any of the known
  ;; shr-based modes.
  (let* ((mode (derived-mode-p '(nov-mode
                                 eww-mode
                                 markdown-mode)))
         (bullet-regexp (when (memq mode '(nov-mode eww-mode))
                          (rx-to-string `(and bol (group (*? blank)) ,shr-bullet))))
         (remove-zws (rx-to-string `(and (group (* space))
                                         (any ,akirak-capture-zero-width-characters)
                                         (group (* space))))))
    (with-temp-buffer
      (insert string)
      (when (and (eq mode 'markdown-mode)
                 (executable-find "pandoc"))
        (call-process-region (point-min) (point-max)
                             "pandoc" t t t
                             "-f" "markdown" "-t" "org" "-"))
      (goto-char (point-min))
      ;; Remove spaces at the beginning of the buffer.
      (when (looking-at (rx (+ (and (* blank) "\n"))))
        (replace-match ""))
      ;; Replace bullets.
      (when bullet-regexp
        (while (re-search-forward bullet-regexp nil t)
          (replace-match "\\1- ")))
      ;; Remove all zero-width whitespaces preceding/following normal space.
      (goto-char (point-min))
      (while (re-search-forward remove-zws nil t)
        (replace-match "\\1\\2"))
      ;; Remove blanks at the end of each line.
      (goto-char (point-min))
      (while (re-search-forward (rx (+ blank) eol) nil t)
        (replace-match ""))
      ;; Remove spaces at the end of the buffer.
      (when (re-search-forward (rx (+ space) eos) nil t)
        (replace-match ""))
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode)))
      (goto-char (point-min))
      (while (looking-at (rx anything))
        (org-fill-paragraph)
        (forward-paragraph))
      (buffer-string))))

(defun akirak-capture--sanitize-source (string)
  ;; Replace zero-width space.
  (cl-flet
      ((indent (s)
         (when (string-match (rx bol (group (+ " ")) (not (any space))) s)
           (- (match-end 1)
              (match-beginning 1)))))
    (let* ((string (thread-last
                     (replace-regexp-in-string
                      (rx-to-string `(and bol (group (* blank))
                                          (any ,akirak-capture-zero-width-characters)
                                          (+ blank)))
                      "" string nil nil 1)
                     (replace-regexp-in-string
                      (rx-to-string `(and (+ (any blank ,akirak-capture-zero-width-characters))
                                          eol))
                      "")
                     (replace-regexp-in-string (rx (+ "\n") eos)
                                               "")
                     (replace-regexp-in-string (rx (+ "\n") eos)
                                               "")
                     (replace-regexp-in-string (rx bos (* space) eol)
                                               "")))
           (lines (split-string string "\n"))
           (indents (thread-last
                      (mapcar #'indent lines)
                      (delq nil)))
           (regexp (when indents
                     (concat "^" (make-string (apply #'min indents)
                                              ?\s)))))
      (if regexp
          (replace-regexp-in-string regexp "" string)
        string))))

;;;###autoload
(defun akirak-capture-sanitize-region (begin end)
  (interactive "r")
  (let ((result (akirak-capture--sanitize-source
                 (buffer-substring begin end))))
    (delete-region begin end)
    (save-excursion
      (goto-char begin)
      (insert result))))

(defun akirak-capture--major-mode-list ()
  (let (modes)
    (cl-do-all-symbols (sym)
      (when (and (fboundp sym)
                 (commandp sym)
                 (string-suffix-p "-mode" (symbol-name sym))
                 (not (or (memq sym minor-mode-list)
                          (memq sym global-minor-modes))))
        (push (thread-last
                (symbol-name sym)
                (string-remove-suffix "-mode")
                (string-remove-suffix "-ts"))
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
                       (string-remove-suffix "-ts")
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

;;;###autoload
(cl-defun akirak-capture-quick-translation (word &key (dest-language "English"))
  (interactive "sWord or phrase: ")
  (let* ((file (oref (or (org-dog-find-file-object
                          (org-dog-file-pred-1
                           `(relative ,(format "languages/%s/vocabulary.org"
                                               dest-language))))
                         (error "Failed to locate the file"))
                     absolute))
         (prompt (format "What are some translations of %s? Provide a word list\
 in a plain Markdown list. Also, describe each word concisely. You don't have \
to quote words." word))
         ;; I don't have an insight on what this system prompt should be.
         (system-prompt (format "You are a large language model and an \
interpreter who are good at %s. Please respond concisely." dest-language))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(akirak-org-capture-make-entry-body
                                (format "Translation of %s" word)
                                :body "%?")
                   :file ,file
                   :headline "Backlog"))))))
    (org-capture)
    (gptel-request prompt :in-place t :system system-prompt)))

;;;; Helper functions

(defun akirak-capture--region-text ()
  (cond
   ((use-region-p)
    (buffer-substring-no-properties
     (region-beginning) (region-end)))
   (akirak-capture-bounds
    (buffer-substring-no-properties
     (car akirak-capture-bounds)
     (cdr akirak-capture-bounds)))))

(defun akirak-capture--first-sentence (text)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (goto-char (cdr (bounds-of-thing-at-point 'sentence)))
    (delete-region (point) (point-max))
    (goto-char (point-min))
    (while (re-search-forward (rx (* blank) "\n" (* blank))
                              nil t)
      (replace-match " "))
    (string-trim (buffer-string))))

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
(cl-defun akirak-capture-clock-in (file headline &key tags (body "%?"))
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
         (tags (ensure-list tags))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(akirak-org-capture-make-entry-body
                                headline
                                :todo "UNDERWAY"
                                :tags (append tags
                                              (akirak-capture--mode-tags file))
                                :properties
                                (akirak-capture-git-properties obj :tags tags)
                                :body body)
                   :file ,file
                   :function ,jump-func
                   :clock-in t :clock-resume t))))))
    (save-window-excursion
      (org-capture))))

(defun akirak-capture--mode-tags (target-file)
  (require 'akirak-org-dog)
  (when-let* ((file (car (akirak-org-dog-context-files 'major-mode)))
              (obj (unless (equal target-file file)
                     (org-dog-file-object file))))
    (org-dog-file-tags obj)))

(cl-defun akirak-capture-git-properties (obj &key tags)
  (when-let* ((root (vc-git-root default-directory)))
    (when (or (member "@contribution" tags)
              (string-prefix-p "projects/" (oref obj relative))
              ;; Prevent mistakenly logging private projects
              (string-prefix-p "~/work2/learning/" root))
      (require 'magit-git)
      (thread-last
        `(("GIT_WORKTREE" . ,(org-link-make-string
                              (concat "file:" (abbreviate-file-name root))))
          ("GIT_ORIGIN" . ,(ignore-errors
                             (car (magit-config-get-from-cached-list
                                   "remote.origin.url"))))
          ("GIT_BRANCH" . ,(ignore-errors
                             (magit-get-current-branch))))
        (seq-filter #'cdr)))))

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
    (if-let* ((marker (cdr (assoc input candidates))))
        (org-goto-marker-or-bmk marker)
      (find-file file)
      (widen)
      (goto-char (point-min))
      (akirak-org-goto-or-create-olp (split-string input "/")))))

(defun akirak-capture--escape-gptel-topic (headline)
  "Just convert a HEADLINE to an Org-property-safe string."
  (thread-last
    (truncate-string-to-width headline 50)
    (replace-regexp-in-string (rx (+ space)) "-")
    (downcase)))

;;;###autoload
(defun akirak-capture-org-ins-heading-fallback (&optional arg)
  "A fallback for `org-insert-heading'.

If `org-insert-heading' should behave as expected, it should
return nil. This is expected in the advice for
`org-insert-heading' defined in akirak-org-clock.el."
  (pcase-let*
      ((`(,direction ,todo) (pcase-exhaustive this-command
                              ;; C-RET
                              (`org-insert-heading-respect-content
                               '(below nil))
                              ;; C-S-RET
                              (`org-insert-todo-heading-respect-content
                               '(below t))
                              ;; M-RET
                              (`org-meta-return
                               '(above nil))
                              ;; M-S-RET
                              (`org-insert-todo-heading
                               '(above t))))
       (pos (point))
       (subheadingp (equal arg '(4)))
       (level (org-outline-level))
       (expected-level (if subheadingp
                           (1+ level)
                         (max level 1)))
       (org-dog-obj (when (featurep 'org-dog)
                      (org-dog-buffer-object)))
       (org-capture-before-finalize-hook nil)
       (org-capture-after-finalize-hook nil)
       (org-capture-prepare-finalize-hook nil)
       (display-buffer-alist '(("^CAPTURE-"
                                akirak-window-display-buffer-split-below)))
       (func (if (= level 0)
                 (cl-ecase direction
                   ;; If there is no heading in the buffer,
                   ;; `outline-next-heading' moves the point to the end of
                   ;; the buffer, so this is sufficient for any cases.
                   (above #'outline-next-heading)
                   ;; Provide no function to add the last top-level heading.
                   (below nil))
               (cl-case direction
                 (above (if subheadingp
                            ;; Insert the heading as the first child of the
                            ;; entry. If there is no subheading of the
                            ;; entry, it will be simply before the next
                            ;; same-level heading.
                            #'outline-next-heading
                          #'org-back-to-heading))
                 (below #'org-end-of-subtree))))
       (org-capture-entry
        (car (doct
              `((""
                 :keys ""
                 :template ,(akirak-org-capture-make-entry-body
                              "%?" :level expected-level)
                 ;; This needs to be plain instead of entry if you
                 ;; specify a concrete level of the heading.
                 :type plain
                 :todo ,(when todo "TODO")
                 :jump-to-captured t
                 :file ,(buffer-file-name (org-base-buffer (current-buffer)))
                 :function
                 (lambda ()
                   (goto-char ,pos)
                   (funcall ',func)
                   (when (looking-at org-heading-regexp)
                     (end-of-line 0)))))))))
    (org-capture)
    t))

(provide 'akirak-capture)
;;; akirak-capture.el ends here
