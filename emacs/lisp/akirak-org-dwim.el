;;; akirak-org-dwim.el --- Transient command for Org -*- lexical-binding: t -*-

(require 'transient)
(require 'octopus)
(require 'akirak-transient)

(declare-function truncate-string-to-width "mule-util")

(defmacro akirak-org-dwim--finalize-capture (&rest progn)
  `(let ((capture-buffer (akirak-org-dwim--capture-buffer org-clock-marker)))
     ,@progn
     (when capture-buffer
       (with-current-buffer capture-buffer
         (org-capture-finalize)))))

;;;; Selected files

(defvar akirak-org-dwim-selected-files nil)

(defun akirak-org-dwim--selected-files ()
  "Return the items of `akirak-org-dwim-selected-files' as a list."
  (if (stringp akirak-org-dwim-selected-files)
      (list akirak-org-dwim-selected-files)
    akirak-org-dwim-selected-files))

;;;; Span

(defcustom akirak-org-dwim-default-span 'day
  ""
  :type '(choice (const day)
                 (const week)))

(defvar akirak-org-dwim-span akirak-org-dwim-default-span)

(defclass akirak-org-dwim-span-class (akirak-transient-variable)
  ())

(transient-define-infix akirak-org-dwim-switch-span ()
  :description "Clock sum"
  :class 'akirak-org-dwim-span-class
  :variable 'akirak-org-dwim-span)

(cl-defmethod transient-infix-read ((obj akirak-org-dwim-span-class))
  (cl-case (oref obj value)
    (day 'week)
    (week 'day)))

(cl-defmethod transient-format ((obj akirak-org-dwim-span-class))
  (let* ((value (oref obj value))
         (range (cl-case value
                  (day (let ((today (org-clock-special-range 'today)))
                         (if (time-less-p (current-time) (car today))
                             (org-clock-special-range 'yesterday)
                           today)))
                  (week (let ((thisweek (org-clock-special-range 'thisweek)))
                          (if (time-less-p (current-time) (car thisweek))
                              (org-clock-special-range 'lastweek)
                            thisweek)))))
         (clocksum (cl-reduce
                    '+
                    (mapcar `(lambda (file)
                               (with-current-buffer (or (find-buffer-visiting file)
                                                        (find-file-noselect file))
                                 (org-with-wide-buffer
                                  (goto-char (point-min))
                                  (org-clock-sum ',(car range) ',(cadr range)))))
                            (akirak-org-dwim--selected-files))
                    :initial-value 0)))
    (format-spec " %k %d (%v) %c"
                 `((?k . ,(transient-format-key obj))
                   (?d . ,(oref obj description))
                   (?v . ,(cl-case value
                            (day "daily")
                            (week "weekly")))
                   (?c . ,(org-minutes-to-clocksum-string clocksum))))))

;;;; Ql search

(defclass akirak-org-dwim-ql-search-param (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-org-dwim-ql-search-param))
  (read--expression (concat (oref obj description) ": ")
                    (prin1-to-string (oref obj value))))

(cl-defmethod transient-format-value ((obj akirak-org-dwim-ql-search-param))
  (if-let (value (oref obj value))
      (concat
       (propertize "(" 'face 'transient-inactive-value)
       (propertize (format "%s" value) 'face 'transient-value)
       (propertize ")" 'face 'transient-inactive-value))
    ""))

;;;;; Query

(defcustom akirak-org-dwim-ql-query-alist
  '(("Project backlog"
     (and (level 2)
          (parent (heading "Backlog"))
          (not (tags "ARCHIVE")))))
  ""
  :type '(alist :key-type string
                :value-type (cons (sexp :tag "Query")
                                  plist
                                  )))

(defvar akirak-org-dwim-ql-query '(todo))

(defvar akirak-org-dwim-ql-query-name nil)

(defun akirak-org-dwim-read-ql-query-name (&optional prompt initial _history)
  (completing-read (or prompt "Query: ")
                   #'akirak-org-dwim-ql-query-completions
                   nil nil initial))

(defun akirak-org-dwim-ql-query-completions (string pred action)
  (if (eq action 'metadata)
      '(metadata . ((annotation-function . akirak-org-dwim-ql-query-annotator)))
    (complete-with-action action
                          (mapcar #'car akirak-org-dwim-ql-query-alist)
                          string
                          pred)))

(defun akirak-org-dwim-ql-query-annotator (group)
  (if-let (plist (cdr (assoc group akirak-org-dwim-ql-query-alist)))
      (concat " " (propertize (prin1-to-string plist)
                              'face 'font-lock-comment-face))
    ""))

(defun akirak-org-dwim-ql-new-search ()
  (interactive)
  (let ((name (akirak-org-dwim-read-ql-query-name)))
    (if-let (cell (assoc name akirak-org-dwim-ql-query-alist))
        (setq akirak-org-dwim-ql-query-name name
              akirak-org-dwim-ql-query (cadr cell))
      (setq akirak-org-dwim-ql-query-name nil
            akirak-org-dwim-ql-query (read name)))
    (akirak-org-dwim-ql-search)))

(defclass akirak-org-dwim-ql-query-class (transient-suffix)
  ())

(cl-defmethod transient-format-description ((obj akirak-org-dwim-ql-query-class))
  (format "%s %s"
          (oref obj description)
          (if-let (value akirak-org-dwim-ql-query-name)
              (concat
               (propertize "(" 'face 'transient-inactive-value)
               (propertize (format "%s" value) 'face 'transient-value)
               (propertize ")" 'face 'transient-inactive-value))
            "")))

(transient-define-suffix akirak-org-dwim-ql-search ()
  :class 'akirak-org-dwim-ql-query-class
  :description "Ql search"
  (interactive)
  (if akirak-org-dwim-ql-query
      (org-ql-search (akirak-org-dwim--selected-files)
        akirak-org-dwim-ql-query
        :super-groups (cdr (assoc akirak-org-dwim-super-groups-name
                                  akirak-org-dwim-super-groups-alist))
        :sort akirak-org-dwim-ql-sort)
    (akirak-org-dwim-ql-new-search)))

;;;;; Super groups

(defvar akirak-org-dwim-super-groups-name nil)

(defclass akirak-org-dwim-super-groups-class (akirak-org-dwim-ql-search-param)
  ())

(defcustom akirak-org-dwim-super-groups-alist
  (when (require 'org-super-agenda nil t)
    (mapcar (lambda (kwd)
              (cons (symbol-name kwd) `((,kwd t))))
            org-super-agenda-auto-selector-keywords))
  ""
  :type '(alist :key-type (string :tag "Description")
                :value-type (repeat plist)))

(cl-defmethod transient-infix-read ((obj akirak-org-dwim-super-groups-class))
  (completing-read "Super groups: "
                   #'akirak-org-dwim-super-groups-completions
                   nil (oref obj value)))

(defun akirak-org-dwim-super-groups-completions (string pred action)
  (if (eq action 'metadata)
      '(metadata . ((annotation-function . akirak-org-dwim-super-group-annotator)))
    (complete-with-action action
                          (mapcar #'car akirak-org-dwim-super-groups-alist)
                          string
                          pred)))

(defun akirak-org-dwim-super-group-annotator (group)
  (if-let (plist (cdr (assoc group akirak-org-dwim-super-groups-alist)))
      (concat " " (propertize (prin1-to-string plist)
                              'face 'font-lock-comment-face))
    ""))

(transient-define-infix akirak-org-dwim-super-groups ()
  :description "Super Groups"
  :class 'akirak-org-dwim-super-groups-class
  :variable 'akirak-org-dwim-super-groups-name)

;;;;; Sort

(defvar akirak-org-dwim-ql-sort nil)

(defclass akirak-org-dwim-ql-sort-class (akirak-org-dwim-ql-search-param)
  ())

(cl-defmethod transient-infix-read ((obj akirak-org-dwim-ql-sort-class))
  (org-ql-view--complete-sort))

(transient-define-infix akirak-org-dwim-ql-sort ()
  :description "Ql Sort"
  :class 'akirak-org-dwim-ql-sort-class
  :variable 'akirak-org-dwim-ql-sort)

;;;; Prefix

;;;###autoload (autoload 'akirak-org-dwim-on-clock "akirak-org-dwim" nil 'interactive)
(transient-define-prefix akirak-org-dwim-on-clock ()
  [:description
   akirak-org-dwim--clock-description
   :class transient-row
   :if org-clocking-p
   ("d" "Mark as done" akirak-org-dwim-clock-done)
   ("r" "Mark as REVIEW" akirak-org-dwim-clock-set-review)
   ("O" "Clock out" akirak-org-dwim-clock-out)
   ("o" "Display clocked entry" akirak-org-dwim-clock-open)
   ;; Save the current window configuration
   ]

  [:description
   akirak-org-dwim--files-description
   :if akirak-org-dwim--files-p
   [("-s" akirak-org-dwim-switch-span)
    ("-g" akirak-org-dwim-super-groups)
    ("-o" akirak-org-dwim-ql-sort)]
   [("SPC" "Clock in" akirak-org-dwim-clock-in-todo)
    ("a" "Agenda" akirak-org-dwim-agenda)
    ("v" akirak-org-dwim-ql-search)
    ("V" "Ql search with new query" akirak-org-dwim-ql-new-search)]]

  [:description
   akirak-org-dwim--memento-block-description
   :if akirak-org-dwim--memento-current-block-p
   :class transient-row
   ("C-o" "Finish" org-memento-finish-block)
   ("C-c" "Stop" org-memento-stop-block)]

  [:description
   akirak-org-dwim--memento-description
   :if akirak-org-dwim--memento-p
   :class transient-row
   ("t" "Open today" org-memento-open-today)
   ("T" "Timeline" org-memento-timeline)
   ("<tab>" "Start a block" org-memento-start-block
    :if-not akirak-org-dwim--memento-current-block-p)
   ("S" "Update status" org-memento-status)
   ("C" "Capture" org-memento-capture)
   ("E" "Check out from the day" org-memento-checkout-from-day
    :if-not akirak-org-dwim--memento-current-block-p)]

  ["Switch focus"
   :if-not org-clocking-p
   :class transient-subgroups
   [:class
    transient-row
    ("x" "Add descendants" akirak-org-dwim-expand-selected-files)
    ("n" "Select a subset" akirak-org-dwim-narrow-selected-files)
    ("j" "Last clocked file" akirak-org-dwim-select-recent-file)
    ("/" "Agenda files" akirak-org-dwim-select-agenda-files)
    ("A" "All agenda files" akirak-org-dwim-select-all-agenda-files)
    ("\\" "This file" akirak-org-dwim-select-this-file
     :if (lambda () (derived-mode-p 'org-mode)))]
   [:class
    transient-columns
    :setup-children octopus-setup-context-file-subgroups]]

  ["Clock in"
   :class transient-row
   ("I" "Last entry" org-clock-in-last)
   ("P" "Project" akirak-org-clock-in-to-project)
   ("J" "History" akirak-consult-org-clock)]

  (interactive)
  (transient-setup 'akirak-org-dwim-on-clock))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-org-dwim-on-clock))
                                 file)
  (setq akirak-org-dwim-selected-files file)
  (transient-setup 'akirak-org-dwim-on-clock))

;;;; Descriptions and predicates

(defun akirak-org-dwim--clock-description ()
  (org-with-clock-position (list org-clock-marker)
    (let ((olp (org-get-outline-path t)))
      (format "Clock: \"%s\" in %s (%s)"
              (car (last olp))
              (buffer-name)
              (substring-no-properties
               (org-format-outline-path (butlast olp)))))))

(defun akirak-org-dwim--files-p ()
  (and akirak-org-dwim-selected-files
       (not (org-clocking-p))))

(defun akirak-org-dwim--files-description ()
  (let ((files (thread-last
                 (akirak-org-dwim--selected-files)
                 (mapcar #'file-name-base))))
    (truncate-string-to-width
     (concat "Selected files: " (string-join files " "))
     (frame-width)
     nil nil (truncate-string-ellipsis))))

(defun akirak-org-dwim--memento-p ()
  (and (require 'org-memento nil t)
       (or org-memento-status-data
           (progn
             (org-memento-status)
             t))))

(defun akirak-org-dwim--memento-description ()
  (concat "Memento: "
          (if-let (day (org-memento-today-as-block))
              (let* ((started (org-memento-started-time day))
                     (ending (org-memento-ending-time day)))
                (format-spec "%d %s%e%r"
                             `((?d . ,(format-time-string "%F (%a)"))
                               (?s . ,(format-time-string "%R" started))
                               (?e . ,(if ending
                                          (format-time-string "-%R" ending)
                                        ""))
                               (?r . ,(if ending
                                          (format " (remaining %s)"
                                                  (org-duration-from-minutes
                                                   (/ (- ending (float-time))
                                                      60)))
                                        "")))))
            "(not checked in)")))

(defun akirak-org-dwim--memento-current-block-p ()
  (bound-and-true-p org-memento-current-block))

(defun akirak-org-dwim--memento-block-description ()
  (let ((block (org-memento-with-current-block
                 (org-memento-block-entry))))
    (format "Memento block: %s%s%s%s"
            (if-let (todo (org-element-property :TODO
                                                (org-memento-headline-element block)))
                (concat todo " ")
              "")
            org-memento-current-block
            (if-let (category (org-element-property :memento_category
                                                    (org-memento-headline-element block)))
                (format " (%s)" category)
              "")
            (let* ((started (org-memento-started-time block))
                   (ending (org-memento-ending-time block))
                   (remaining (when ending
                                (/ (- ending (float-time))
                                   60))))
              (format-spec " %s%e%r"
                           `((?s . ,(format-time-string "%R" started))
                             (?e . ,(if ending
                                        (format-time-string "-%R" ending)
                                      ""))
                             (?r . ,(cond
                                     ((null remaining)
                                      "")
                                     ((> remaining 0)
                                      (format " (remaining %s)"
                                              (org-duration-from-minutes remaining)))
                                     ((<= remaining 0)
                                      (propertize (format " (%d minutes exceeding)"
                                                          (- remaining))
                                                  'face
                                                  (if (> remaining 0)
                                                      'font-lock-warning-face
                                                    'default)))))))))))

(defun akirak-org-dwim--memento-status-description ()
  (format "No current block"))

;;;; Selecting files

(defun akirak-org-dwim-select-agenda-files ()
  (interactive)
  (setq akirak-org-dwim-selected-files
        (completing-read-multiple
         "Select files: "
         (org-dog-file-completion :files org-agenda-files)))
  (transient-setup 'akirak-org-dwim-on-clock))

(defun akirak-org-dwim-select-all-agenda-files ()
  (interactive)
  (org-dog-root-add-active-files (cl-case akirak-org-dwim-span
                                   (week 7)
                                   (day 1)))
  (setq akirak-org-dwim-selected-files org-agenda-files)
  (transient-setup 'akirak-org-dwim-on-clock))

(defun akirak-org-dwim-expand-selected-files ()
  (interactive)
  (setq akirak-org-dwim-selected-files
        (mapcar #'car (org-dog-overview-scan
                       (akirak-org-dwim--selected-files)
                       :fast t)))
  (transient-setup 'akirak-org-dwim-on-clock))

(defun akirak-org-dwim-narrow-selected-files ()
  (interactive)
  (setq akirak-org-dwim-selected-files
        (completing-read-multiple
         "Select files: "
         (org-dog-file-completion :files (akirak-org-dwim--selected-files))))
  (transient-setup 'akirak-org-dwim-on-clock))

(defun akirak-org-dwim-select-recent-file ()
  (interactive)
  (when-let (marker (car org-clock-history))
    (setq akirak-org-dwim-selected-files
          (abbreviate-file-name (buffer-file-name (marker-buffer marker))))
    (transient-setup 'akirak-org-dwim-on-clock)))

(defun akirak-org-dwim-select-this-file ()
  (interactive)
  (setq akirak-org-dwim-selected-files (abbreviate-file-name (buffer-file-name)))
  (transient-setup 'akirak-org-dwim-on-clock))

;;;; Suffix commands

(defun akirak-org-dwim-clock-in-todo ()
  (interactive)
  (org-dog-clock-in akirak-org-dwim-selected-files
                    :query-prefix "todo: "))

(defun akirak-org-dwim-agenda ()
  (interactive)
  (let ((org-agenda-files (akirak-org-dwim--selected-files))
        (org-agenda-span akirak-org-dwim-span)
        (org-agenda-start-with-clockreport-mode t)
        (org-agenda-use-time-grid t))
    (org-agenda nil "a")))

(defun akirak-org-dwim-clock-out ()
  (interactive)
  (if-let (capture-buffer (akirak-org-dwim--capture-buffer org-clock-marker))
      (with-current-buffer capture-buffer
        (org-capture-finalize))
    (org-clock-out)))

(defun akirak-org-dwim-clock-done ()
  (interactive)
  (org-with-clock-position (list org-clock-marker)
    (akirak-org-dwim--finalize-capture
     (org-todo 'done))))

(defun akirak-org-dwim-clock-set-review ()
  (interactive)
  (org-with-clock-position (list org-clock-marker)
    (akirak-org-dwim--finalize-capture
     ;; If you add the todo keyword to `org-clock-out-when-done', `org-clock-out'
     ;; will be tirggered when you switch to the state.
     (org-todo "REVIEW"))))

;;;###autoload
(defun akirak-org-dwim-clock-open ()
  (interactive)
  (if-let (capture-buffer (akirak-org-dwim--capture-buffer org-clock-marker))
      (pop-to-buffer capture-buffer)
    (with-current-buffer (marker-buffer org-clock-marker)
      (let ((initial-position (point)))
        (goto-char org-clock-marker)
        (with-current-buffer (org-dog-indirect-buffer)
          (when (or (< (point) (point-min))
                    (> (point) (point-max)))
            (goto-char (point-min)))
          (pop-to-buffer (current-buffer)))))))

;;;; Integration with org-capture

(defun akirak-org-dwim--capture-buffer (clock-marker)
  "Return a corresponding capture buffer for the clock marker."
  (let ((suffix (buffer-name (marker-buffer clock-marker)))
        (point (org-with-clock-position (list clock-marker)
                 (org-back-to-heading)
                 (point))))
    (thread-last
      (internal-complete-buffer "CAPTURE-" nil t)
      (seq-some `(lambda (name)
                   (when (string-suffix-p ,suffix name)
                     (with-current-buffer (get-buffer name)
                       (when (eq (point-min) ,point)
                         (current-buffer)))))))))

(provide 'akirak-org-dwim)
;;; akirak-org-dwim.el ends here
