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

(defvar akirak-capture-hook nil)

(defvar akirak-capture-new-tab nil)

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

(defvar akirak-capture-example-type nil)

(transient-define-infix akirak-capture-example-source-infix ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :description "Source"
  :choices '(kill-ring minibuffer)
  :variable 'akirak-capture-example-type)

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
   ("-I" akirak-capture-doct-immediate-finish)
   ("-a" akirak-capture-doct-add-annotation)]
  ["Gptel"
   :class transient-row
   :if-non-nil akirak-capture-gptel-topic
   ("-c" akirak-capture-dispatch-later)
   (gptel--infix-provider)]
  ["Context"
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-doct (octopus-generate-context-file-subgroups)))]
  ["Static files"
   :class transient-row
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-doct (octopus-generate-static-targets)))]
  ["Other locations"
   :class transient-row
   ("'" octopus-avy-org-heading-suffix)
   ("@" octopus-clock-marker-suffix)
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)
   ("$" octopus-last-captured-file-suffix)
   ("%" akirak-capture-org-clock-history-suffix)]
  (interactive)
  ;; Load gptel--infix-provider
  (require 'gptel-transient)
  (transient-setup 'akirak-capture-doct))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-doct))
                                 target)
  (let* ((template-options akirak-capture-template-options)
         ;; When this variable is non-nil, start the capture session in a new
         ;; tab.
         (new-tab-name (when akirak-capture-new-tab
                         akirak-capture-headline))
         (doct-options (if (or new-tab-name
                               akirak-capture-hook)
                           (thread-first
                             akirak-capture-doct-options
                             (plist-put :hook
                                        (when akirak-capture-hook
                                          `(lambda ()
                                             ;; Set some delay for Emacs to initialize the buffer.
                                             (run-with-timer
                                              0.1
                                              nil
                                              (lambda ()
                                                ,@(when new-tab-name
                                                    `((tab-bar-rename-tab ,new-tab-name)
                                                      (when (fboundp 'fwb-toggle-window-split)
                                                        (fwb-toggle-window-split))))
                                                ,(when (and akirak-capture-hook
                                                            (not akirak-capture-dispatch-later))
                                                   `(funcall ',akirak-capture-hook)))))))
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
                    ;; olp target doesn't work if one of the headlines is a
                    ;; link, so use `org-goto-marker-or-bmk' instead.
                    :function `(lambda ()
                                 (org-goto-marker-or-bmk ',target))))
      (string (list :file target
                    :function jump-func))
      (org-dog-file (list :file (oref target absolute)
                          :function jump-func)))))

;;;; Transient prefixes

(defvar akirak-capture-start-now nil)

(transient-define-infix akirak-capture-toggle-start-now ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-capture-start-now
  :description "Start now")

(defun akirak-capture--template-options ()
  (list :todo (if akirak-capture-start-now
                  "UNDERWAY"
                "TODO")))

(defun akirak-capture--doct-options ()
  (if akirak-capture-start-now
      '(:clock-in t :clock-resume t)
    nil))

;;;###autoload (autoload 'akirak-capture "akirak-capture" nil 'interactive)
(transient-define-prefix akirak-capture (&optional initial)
  "Main entry point to capture commands."
  ["Options"
   ("-n" akirak-capture-toggle-start-now)]
  ["Non-contextual commands"
   :class transient-row
   ("h" "Plain heading"
    (lambda ()
      (interactive)
      (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
            akirak-capture-template-options nil
            akirak-capture-doct-options (akirak-capture--doct-options))
      (akirak-capture-doct))
    :transient t)
   ("t" "Todo" (lambda ()
                 (interactive)
                 (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                       akirak-capture-template-options (akirak-capture--template-options)
                       akirak-capture-doct-options (akirak-capture--doct-options))
                 (akirak-capture-doct))
    :transient t)
   ("u" "Url" akirak-capture-url
    :if (lambda () (not akirak-capture-initial)))
   ("g" "Gptel" akirak-capture-gptel
    :transient t)
   ("r" "Research" (lambda ()
                     (interactive)
                     (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
                           akirak-capture-template-options '(:tags "@question")
                           akirak-capture-doct-options '(:clock-in t :clock-resume t))
                     (akirak-capture-doct)))
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
   ("l" "Link" (lambda ()
                 (interactive)
                 (let ((inhibit-message t))
                   (call-interactively #'org-store-link))
                 (setq akirak-capture-headline
                       (org-link-make-string
                        (car (pop org-stored-links))
                        (read-string "Description: " (which-function)))
                       akirak-capture-template-options nil
                       akirak-capture-doct-options nil)
                 (akirak-capture-doct))
    :transient t)]

  ["Contextual"
   :class transient-row
   ("e" "Flymake error" akirak-capture-flymake-error-at-point
    :if akirak-capture--at-error-p)

   (">" "Project task" akirak-capture-project-context
    :if project-current)

   ("!" "Project task (no context)" akirak-capture-project-task
    :if project-current)

   ("?" "Project inquiry" akirak-capture-project-inquiry
    :transient t
    :if project-current)

   ("%" "Troubleshooting" akirak-capture-issue)]

  ["Append to clock"
   :class transient-row
   :if org-clocking-p
   ("=" "Append heading to clock" akirak-capture-append-heading-to-clock)
   ("<" "Append block to clock" akirak-capture-append-block-to-clock
    :if use-region-p)
   (">" "Convert to a link to a new entry" akirak-org-convert-to-entry-link
    :if (lambda ()
          (and (use-region-p)
               (derived-mode-p 'org-mode))))]

  ["Convenience and specific projects"
   :class transient-row
   ("e" "Emacs config" akirak-emacs-config-capture)
   ("sn" "Snippet" akirak-capture-snippet
    :if use-region-p)]

  (interactive)
  (if (equal current-prefix-arg '(16))
      (org-capture-goto-last-stored)
    (setq akirak-capture-initial initial
          akirak-capture-new-tab nil
          akirak-capture-hook nil)
    (when akirak-capture-initial
      (message "Heading set to \"%s\"" akirak-capture-initial))
    (setq akirak-capture-bounds (when (use-region-p)
                                  (car (region-bounds))))
    (setq akirak-capture-start-now nil)
    (transient-setup 'akirak-capture)))

(defun akirak-capture--maybe-read-heading (&optional prompt)
  (or akirak-capture-initial
      (akirak-capture-read-string (or prompt "Heading: "))))

(defun akirak-capture--make-org-link ()
  (call-interactively #'org-store-link)
  (let ((link (pop org-stored-links)))
    (org-link-make-string (car link)
                          (read-from-minibuffer "Description: "
                                                (cadr link)))))

(defun akirak-capture--org-global-tags ()
  (thread-last
    (mapcar #'car org-tag-persistent-alist)
    (cl-remove-if-not #'stringp)))

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
  (setq akirak-capture-new-tab nil)
  (akirak-capture-doct))

(defun akirak-capture-flymake-error-at-point ()
  (interactive)
  (require 'akirak-flymake)
  (pcase-let* ((diag (pcase (flymake-diagnostics)
                       (`nil (user-error "No error at point"))
                       (`(,diag) diag)
                       (diags (akirak-flymake-select-diagnostic diags))))
               (line (line-number-at-pos (flymake-diagnostic-beg obj)))
               (col (car (posn-col-row (posn-at-point (flymake-diagnostic-beg obj)))))
               (file (buffer-file-name (or (buffer-base-buffer)
                                           (current-buffer))))
               (prompt (read-string
                        "Prompt: "
                        (format-spec
                         "Investigate the error at line %l in %f:"
                         `((?l . ,line)
                           (?f . ,(file-relative-name file (vc-git-root file)))))))
               (text (flymake-diagnostic-text diag)))
    (setq akirak-capture-headline (thread-last
                                    (split-string (string-trim text) "\n")
                                    (cl-remove-if #'string-empty-p)
                                    (car)
                                    (string-trim))
          akirak-capture-template-options (list :todo "UNDERWAY"
                                                :tags '("@troubleshooting")
                                                :body
                                                (concat prompt
                                                        "\n\n#+begin_example\n"
                                                        text
                                                        "\n#+end_example"
                                                        "\n\n%?"))
          akirak-capture-doct-options '(:clock-in t :clock-resume t))
    (akirak-capture-doct)))

(defun akirak-capture--at-error-p ()
  (and (get-char-property-and-overlay (point) 'flymake-diagnostic)
       t))

(defun akirak-capture-project-context ()
  (interactive)
  (require 'akirak-org-git)
  (let* ((filename (buffer-file-name (or (buffer-base-buffer)
                                         (current-buffer))))
         (contribution (string-match-p (regexp-quote "/contributions/")
                                       filename))
         (file (when filename
                 (file-relative-name filename (vc-git-root filename))))
         (bounds (when (use-region-p)
                   (cons (region-beginning) (region-end))))
         (func (when file
                 (which-function)))
         (prompts (when func
                    (list (format-spec "Tweak ~%n~ in %f to "
                                       `((?n . ,func)
                                         (?f . ,file)))
                          (format-spec "Fix ~%n~ in %f to "
                                       `((?n . ,func)
                                         (?f . ,file)))
                          (format-spec "Refactor ~%n~ in %f to "
                                       `((?n . ,func)
                                         (?f . ,file))))))
         (prompt (completing-read "Prompt: " prompts))
         (troubleshooting (and bounds (not file)))
         (clock-in troubleshooting)
         (include-file (and file
                            (string-match-p (regexp-quote file) prompt)))
         (include-func (and func
                            (string-match-p (regexp-quote func) prompt)))
         (text (when bounds
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (setq akirak-capture-headline (if (and troubleshooting text)
                                      (thread-last
                                        (split-string (string-trim text) "\n")
                                        (cl-remove-if #'string-empty-p)
                                        (car))
                                    "")
          akirak-capture-template-options
          (append (list :tags (cond
                               (contribution
                                '("@contribution"))
                               (troubleshooting
                                '("@troubleshooting")))
                        :properties
                        (akirak-org-git-properties t
                          :no-branch (not akirak-capture-start-now)
                          :include-file include-file)
                        :body
                        (concat prompt
                                (when bounds
                                  (if file
                                      (concat "\n\n#+begin_src\n"
                                              text
                                              "\n#+end_src")
                                    (concat "\n\n#+begin_example\n"
                                            text
                                            "\n#+end_example")))
                                "\n\n%?"
                                (cond
                                 (include-func
                                  "\n\n# %a")
                                 ((bound-and-true-p compilation-shell-minor-mode)
                                  (format "\n\n# compile: %s" compile-command)))))
                  (akirak-capture--template-options))
          akirak-capture-doct-options (if clock-in
                                          '(:clock-in t :clock-resume t)
                                        (akirak-capture--doct-options)))
    (akirak-capture-doct)))

(defun akirak-capture-project-task ()
  (interactive)
  (require 'akirak-org-git)
  (let* ((filename (buffer-file-name (buffer-base-buffer)))
         (contribution (string-match-p (regexp-quote "/contributions/")
                                       (or filename
                                           default-directory))))
    (setq akirak-capture-headline (akirak-capture--maybe-read-heading)
          akirak-capture-template-options (append (list :properties
                                                        (akirak-org-git-properties t
                                                          :no-branch (not akirak-capture-start-now)
                                                          :include-file nil)
                                                        :tags
                                                        (when contribution
                                                          '("@contribution"))
                                                        :body "%?")
                                                  (akirak-capture--template-options))
          akirak-capture-doct-options (akirak-capture--doct-options))
    (akirak-capture-doct)))

(defun akirak-capture-project-inquiry ()
  (interactive)
  (require 'akirak-org-git)
  (setq akirak-capture-gptel-topic nil
        akirak-capture-dispatch-later nil
        akirak-capture-headline (akirak-capture--maybe-read-heading)
        akirak-capture-doct-options nil
        akirak-capture-template-options (list :properties (akirak-org-git-properties t))
        akirak-capture-new-tab t
        akirak-capture-hook #'akirak-org-claude-explain)
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
  ["Context"
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-snippet (octopus-generate-context-file-subgroups)))]
  ["Other locations"
   :class transient-row
   ("\\" octopus-this-file-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive "r")
  (transient-setup 'akirak-capture-snippet))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-snippet))
                                 target)
  (require 'akirak-tempo)
  (let ((org-capture-entry
         (car (doct
               `((""
                  :keys ""
                  :template ,(akirak-org-capture-make-entry-body
                               (read-string "Heading for the snippet entry: ")
                               :body
                               (concat "#+begin_src tempo\n%?"
                                       (thread-last
                                         (buffer-substring-no-properties
                                          (region-beginning) (region-end))
                                         (replace-regexp-in-string "%" "%%")
                                         (akirak-tempo-from-string))
                                       "\n#+end_src"))
                  :function akirak-capture--goto-snippets
                  :after-finalize akirak-org-tempo-add-entry
                  ,@(akirak-capture--target-plist target)))))))
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

(defun akirak-capture--heading-capture-children ()
  (let (result
        (min-level (org-element-property
                    :level (org-element-at-point-no-context org-clock-hd-marker))))
    (dolist (n (number-sequence min-level 9))
      (push (list (int-to-string n)
                  (format "Level %d" n)
                  (cdr (assq n akirak-capture-heading-commands)))
            result))
    (nreverse result)))

(transient-define-prefix akirak-capture-issue ()
  :refresh-suffixes t
  ["Options"
   :class transient-row
   ("s" akirak-capture-example-source-infix)]
  ["Context"
   :class transient-columns
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-issue (octopus-generate-context-file-subgroups)))   ]
  ["Static files"
   :class transient-row
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-issue (octopus-generate-static-targets)))]
  ["Other locations"
   :class transient-row
   ("@" octopus-clock-marker-suffix)
   ("/" octopus-read-dog-file-suffix)]
  (interactive)
  (setq akirak-capture-example-type 'kill-ring)
  (transient-setup 'akirak-capture-issue))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-capture-issue))
                                 target)
  (let* ((text (pcase-exhaustive akirak-capture-example-type
                 (`kill-ring
                  (read-from-kill-ring "Quoted text (from kill ring): "))
                 (`minibuffer
                  (read-string "Quoted text: "))))
         (headline (thread-last
                     (string-split text "\n")
                     (cl-remove-if #'string-empty-p)
                     (akirak-completing-read-no-sorted "Headline: ")))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :template ,(akirak-org-capture-make-entry-body
                                headline
                                :todo "UNDERWAY"
                                :tags '("@troubleshooting")
                                :properties nil
                                :body
                                (concat (if (org-clocking-p)
                                            (org-with-point-at org-clock-marker
                                              (concat ":METADATA:\n"
                                                      "Source: "
                                                      (akirak-capture--clocked-entry-link)
                                                      "\n:END:\n"))
                                          "")
                                        "%?\n\n"
                                        "#+begin_example\n"
                                        (string-trim-right text)
                                        "\n#+end_example\n"))
                   :clock-in t :clock-resume t
                   ,@(akirak-capture--target-plist target)))))))
    (org-capture)))

(defun akirak-capture--clocked-entry-link ()
  "Return an Org link to the clocked entry."
  (org-with-point-at org-clock-marker
    (org-link-make-string (concat "id:" (org-id-get-create))
                          (org-entry-get nil "ITEM"))))

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
   :setup-children
   (lambda (children)
     (append children
             (transient-parse-suffixes 'akirak-capture-append-heading-to-clock
                                       (akirak-capture--heading-capture-children))))
   ("=" "Same level" akirak-capture--same-level-heading)
   ("+" "Subheading" akirak-capture--subheading)]
  (interactive (list (thread-last
                       (cond
                        ((use-region-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
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
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-url (octopus-generate-context-file-subgroups)))]
  ["Static files"
   :class transient-row
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-url (octopus-generate-static-targets)))]
  ["Other locations"
   :class transient-row
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
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-journal (octopus-generate-context-file-subgroups)))]
  ["Static files"
   :class transient-row
   :setup-children
   (lambda (_)
     (transient-parse-suffixes 'akirak-capture-journal (octopus-generate-static-targets)))]
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
  (require 'gptel-org)
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
            akirak-capture-new-tab t
            akirak-capture-dispatch-later dispatch-later
            akirak-capture-headline headline
            akirak-capture-hook #'gptel-send
            akirak-capture-doct-options nil
            akirak-capture-template-options (list :tags "@AI"
                                                  :properties
                                                  `(("GPTEL_TOPIC"
                                                     . ,(akirak-capture--escape-gptel-topic
                                                         headline)))
                                                  :body
                                                  (concat preamble llm-prompt
                                                          (unless dispatch-later
                                                            "%?"))))
      (akirak-capture-doct))))

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
                                               (akirak-org-src-lang-at-point)
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
      (replace-regexp-in-string
       (rx bol "*")
       ",*"
       (if regexp
           (replace-regexp-in-string regexp "" string)
         string)))))

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

(defun akirak-capture--goto-backlog ()
  (widen)
  (goto-char (point-min))
  (akirak-org-goto-or-create-olp '("Backlog")))

(defun akirak-capture--goto-snippets ()
  (widen)
  (goto-char (point-min))
  (akirak-org-goto-or-create-olp '("Snippets")))

(defun akirak-capture--datetree-marker (file)
  "Return a marker to the current date in FILE."
  (with-current-buffer (or (find-buffer-visiting file)
                           (find-file-noselect file))
    (org-reverse-datetree-goto-date-in-file nil :return 'marker)))

;;;###autoload
(cl-defun akirak-capture-clock-in (file headline &key tags (body "%?"))
  "Create a new heading with a title and clock into it.

This is intended as the value of `org-dog-clock-in-fallback-fn'."
  (require 'akirak-org-git)
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
                                (akirak-org-git-properties obj :tags tags)
                                :body body)
                   :file ,file
                   :function ,jump-func
                   :clock-in t :clock-resume t)))))
         (org-clock-auto-clock-resolution nil))
    (save-window-excursion
      (org-capture))))

(defun akirak-capture--mode-tags (target-file)
  (require 'akirak-org-dog)
  (when-let* ((file (car (akirak-org-dog-context-files 'major-mode)))
              (obj (unless (equal target-file file)
                     (org-dog-file-object file))))
    (org-dog-file-tags obj)))

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
    (truncate-string-to-width (if (string-match org-link-bracket-re headline)
                                  (or (match-string 2 headline)
                                      (match-string 1 headline))
                                headline)
                              50)
    (replace-regexp-in-string (rx (+ space)) "-")
    (downcase)))

;;;###autoload
(defun akirak-capture-org-ins-heading-fallback (&optional arg)
  "A fallback for `org-insert-heading'.

If `org-insert-heading' should behave as expected, it should
return nil. This is expected in the advice for
`org-insert-heading' defined in akirak-org-clock.el."
  (pcase-exhaustive this-command
    ;; C-RET
    (`org-insert-heading-respect-content
     (akirak-capture--org-heading 'below nil arg))
    ;; C-S-RET
    (`org-insert-todo-heading-respect-content
     (akirak-capture--org-heading 'below t arg))
    ;; M-RET
    (`org-meta-return
     (when (org-match-line org-heading-regexp)
       (akirak-capture--org-heading 'above nil arg)))
    ;; M-S-RET
    (`org-insert-todo-heading
     (when (org-match-line org-heading-regexp)
       (akirak-capture--org-heading 'above t arg)))))

(defun akirak-capture--org-heading (direction todo &optional arg)
  (pcase-let*
      ((pos (point))
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
