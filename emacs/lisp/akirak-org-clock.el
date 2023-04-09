;;; akirak-org-clock.el ---  -*- lexical-binding: t -*-

(require 'org-clock)
(require 'thunk)

(defvar akirak-emacs-org-config-file)

(defcustom akirak-org-clock-history-threshold (* 3600 24 2)
  "Number of seconds for which you want to take account for clock
 activities."
  :type 'number)

(defmacro akirak-org-clock--finalize-capture (&rest progn)
  `(let ((capture-buffer (akirak-org-clock--capture-buffer org-clock-marker)))
     ,@progn
     (when capture-buffer
       (with-current-buffer capture-buffer
         (org-capture-finalize)))))

(defcustom akirak-org-clock-target nil
  "(FILES QUERY-PREFIX TAG FURTHER)."
  :local t
  :type '(list (repeat filename)
               string
               string
               boolean))

;;;; Global mode to ensure clocking

(defvar akirak-org-clock-snooze-until nil)

;;;###autoload
(define-minor-mode akirak-org-clock-mode
  "Ensure clocking"
  :global t
  :lighter " OClk"
  (cond
   ((bound-and-true-p akirak-org-clock-mode)
    (ad-activate 'save-buffer)
    (ad-activate 'org-self-insert-command))
   (t
    (ad-deactivate 'save-buffer)
    (ad-deactivate 'org-self-insert-command))))

(defconst akirak-org-clock-file-name-whitelist
  (rx-to-string `(or (and bol (or ,(expand-file-name user-emacs-directory)
                                  ,(expand-file-name "~/fleeting/")
                                  ,(expand-file-name "~/resources/images/")
                                  ,(expand-file-name "~/resources/articles/")
                                  (and "emacs-config.org" eol)
                                  "/private-config/"
                                  "/tmp"))
                     "/.")))

(defconst akirak-org-clock-buffer-name-whitelist
  ;; Don't block saving buffers created using `with-temp-buffer'
  (rx bos (or " *temp*"
              "CAPTURE-")))

(defsubst akirak-org-clock--snoozed-p ()
  (and akirak-org-clock-snooze-until
       (< (float-time) akirak-org-clock-snooze-until)))

(defadvice save-buffer (around akirak-org-clock activate)
  (require 'akirak-emacs-org)
  (when-let (filename (if-let (base (buffer-base-buffer))
                          (buffer-file-name base)
                        buffer-file-name))
    (when (or (string-match-p akirak-org-clock-buffer-name-whitelist
                              (buffer-name))
              (string-match-p akirak-org-clock-file-name-whitelist
                              filename)
              (and (bound-and-true-p akirak-emacs-org-config-file)
                   (string-equal (expand-file-name akirak-emacs-org-config-file)
                                 filename))
              (bound-and-true-p url-http-content-type)
              (eq this-command 'magit-show-commit)
              (when-let (mode (derived-mode-p 'org-mode 'org-memento-policy-mode))
                (cl-case mode
                  (org-memento-policy-mode t)
                  (org-mode (or (bound-and-true-p org-capture-mode)
                                (and (featurep 'org-dog)
                                     (org-dog-buffer-object))))))
              (akirak-org-clock--snoozed-p)
              (file-remote-p filename)
              (akirak-org-clock--check-before-save))
      ad-do-it)))

(defun akirak-org-clock--check-before-save ()
  (require 'org-clock)
  (require 'akirak-org-dog)
  (require 'org-dog-overview)
  (pcase-exhaustive (akirak-org-clock--target)
    (`(,files ,query-prefix ,tag ,further)
     (or (and (org-clocking-p)
              (let ((filename (thread-last
                                (marker-buffer org-clock-marker)
                                (buffer-file-name)
                                (abbreviate-file-name))))
                (or (member filename files)
                    (when further
                      (member filename
                              (thread-last
                                (org-dog-overview-scan files :fast t)
                                (mapcar #'car))))))
              (or (not tag)
                  (member tag
                          (save-current-buffer
                            (org-with-point-at org-clock-marker
                              (org-get-tags))))))
         (progn
           (require 'org-dog-clock)
           (message "You must clock in")
           (let ((files (if further
                            (thread-last
                              (org-dog-overview-scan files :fast t)
                              (mapcar #'car))
                          files)))
             (if files
                 (progn
                   (org-dog-clock-in files
                                     :query-prefix query-prefix
                                     :tags tag
                                     :prompt
                                     (format "Clock in (%s): "
                                             (mapconcat #'file-name-nondirectory
                                                        files ", ")))
                   t)
               (message "No Org file to clock in to")))
           t)))))

(defun akirak-org-clock--target ()
  (or akirak-org-clock-target
      (pcase (project-root (or (project-current)
                               (if (yes-or-no-p "Not in a project. Run git init?")
                                   (progn
                                     (let ((default-directory (read-directory-name "Run git init at: ")))
                                       (call-process "git" nil nil nil "init"))
                                     (or (project-current)
                                         (user-error "The directory is not inside a project")))
                                 (user-error "Must be in a project"))))
        ((rx "/foss/contributions/")
         (list (list (car (akirak-org-dog-major-mode-files)))
               "tag:@contribution "
               "@contribution"
               nil))
        ((and (rx "/learning/" (group (+ (not (any "/")))) "/")
              (app (match-string 1) category))
         (list (org-dog-select 'absolute
                 `(relative :regexp ,(rx-to-string `(and "/" ,category
                                                         (? "." (+ (not (any "/"))))
                                                         ".org"))))
               ""
               nil
               nil))
        ("~/org/"
         (if (eq major-mode 'org-memento-policy-mode)
             (list (list "~/org/focus.org" "~/org/meta.org")
                   ""
                   nil
                   nil)
           (list (list "~/org/meta.org")
                 "todo: "
                 nil
                 nil)))
        (_
         (list (akirak-org-dog-project-files)
               "todo: "
               nil
               t)))))

;;;###autoload
(defun akirak-org-clock-in-to-project ()
  "Clock in to an entry in a file related to the current project."
  (interactive)
  (require 'akirak-org-dog)
  (pcase-exhaustive (akirak-org-clock--target)
    (`(,files ,query-prefix ,_tag ,further)
     (let ((files (if further
                      (thread-last
                        (org-dog-overview-scan files
                                               :fast t)
                        (mapcar #'car))
                    files)))
       (org-dog-clock-in files
                         :query-prefix query-prefix
                         :prompt
                         (format "Clock in to project file (%s): "
                                 (mapconcat #'file-name-nondirectory
                                            files ", ")))))))

(defun akirak-org-clock--project-name (pr)
  "Return the name of the project for use in prompt."
  (thread-last
    (project-root pr)
    (string-remove-suffix "/")
    (file-name-nondirectory)))

(defcustom akirak-org-clock-snooze-duration 60
  "Duration in seconds of snoozing in Org mode."
  :type 'number)

(defcustom akirak-org-clock-reclock-interval 20
  "Reclocking interval."
  :type 'number)

(defadvice org-self-insert-command (around akirak-org-clock activate)
  (when (or (org-clocking-p)
            (bound-and-true-p org-capture-mode)
            (let ((filename (buffer-file-name (org-base-buffer (current-buffer)))))
              ;; Pass non-file buffers like *Org Note* buffers.
              (or (not filename)
                  (bound-and-true-p org-memento-file-mode)
                  ;; I sometimes edit Org file inside `user-emacs-directory', and
                  ;; I don't want to
                  (string-match-p akirak-org-clock-file-name-whitelist
                                  filename)
                  (if (bound-and-true-p org-dog-file-mode)
                      (or (org-before-first-heading-p)
                          (akirak-org-clock--snoozed-p)
                          (user-error "Please clock-in first"))
                    t))))
    ad-do-it))

;;;###autoload
(defun akirak-org-clock-in-dwim ()
  (interactive)
  (cond
   ((bound-and-true-p org-dog-file-mode)
    (akirak-org-clock-in-to-dog))
   (t
    (akirak-org-clock-in-to-project))))

(defun akirak-org-clock-in-to-dog ()
  (interactive)
  (cl-assert (bound-and-true-p org-dog-file-mode))
  (let* ((link (progn
                 (org-dog-store-file-link)
                 (car (pop org-stored-links))))
         (width (frame-width))
         (candidates (org-ql-select "~/org/meta.org"
                       `(and (todo)
                             (or (ancestors (link nil :target ,link))
                                 (link nil :target ,link)
                                 (parent (heading "General"))))
                       :action `(cons (org-format-outline-path
                                       (org-get-outline-path t t)
                                       ,width
                                       (org-get-todo-state))
                                      (point-marker)))))
    (pcase-exhaustive (cdr (assoc (completing-read "Clock in: " candidates nil t)
                                  candidates))
      ((and marker (cl-type marker))
       (org-clock-clock-in (list marker))))))

;;;###autoload
(defun akirak-org-clock-snooze (&optional seconds)
  (interactive "P")
  (let ((seconds (or (when (numberp seconds)
                       seconds)
                     akirak-org-clock-snooze-duration)))
    (setq akirak-org-clock-snooze-until
          (+ (float-time) seconds))
    (message "Snoozing org clock mode for %s seconds" seconds)
    (add-hook 'org-clock-in #'akiraik-org-clock-stop-snoozing)))

(defun akiraik-org-clock-stop-snoozing ()
  (setq akirak-org-clock-snooze-until nil))

(defun akirak-org-clock-reclock-in ()
  "Reclock in for updating the title."
  (when (org-clocking-p)
    (save-current-buffer
      (org-with-point-at org-clock-marker
        (org-clock-in)))))

;;;; Rebuild the history

;;;###autoload
(defun akirak-org-clock-rebuild-history ()
  (interactive)
  (let ((message-log-max nil))
    (message "Rebuilding the clock history..."))
  (setq org-clock-history
        (seq-take
         (thread-last
           (org-dog-select 'absolute)
           (seq-filter #'akirak-org-clock--recently-active-p)
           (org-map-entries (lambda ()
                              (let ((marker (point-marker))
                                    (time (akirak-org-clock--last-clock-time)))
                                (when time
                                  (cons time marker))))
                            nil)
           (delq nil)
           (seq-sort-by #'car (lambda (x y) (not (time-less-p x y))))
           (mapcar #'cdr))
         org-clock-history-length)))

;;;###autoload
(defun akirak-org-clock-add-agenda-files (&optional days)
  "Add files that have recent activities to `org-agenda-files'."
  (interactive)
  (let ((days (or days (cl-case (or org-agenda-current-span
                                    org-agenda-span)
                         (day 1)
                         (week 7)
                         (month 31)
                         (year 366)
                         (otherwise org-agenda-span))))
        (diff (seq-filter `(lambda (file)
                             (and (not (member file org-agenda-files))
                                  (akirak-org-clock--recently-active-p file ,days)))
                          (org-dog-select 'absolute))))
    (when diff
      (setq org-agenda-files (append org-agenda-files diff))
      (message "Added %d files" (length diff)))))

(defun akirak-org-clock--last-clock-time ()
  (save-match-data
    (let ((bound))
      (when (and (search-forward org-clock-string
                                 (org-entry-end-position) t)
                 (re-search-forward (org-re-timestamp 'inactive)
                                    (line-end-position)
                                    t))
        (let ((time (org-time-string-to-time (match-string 0))))
          (when (< (- (float-time) (float-time time))
                   akirak-org-clock-history-threshold)
            time))))))

(defun akirak-org-clock--date-regxps (&optional days)
  "Return a regexp for inactive clock within a certain DAYS.

DAYS default to `akirak-org-clock-history-threshold'."
  (rx-to-string `(seq (or ,@(mapcar (lambda (days)
                                      (format-time-string
                                       "%F" (+ (float-time) (* 3600 24 days))))
                                    (number-sequence (- (or days
                                                            (/ akirak-org-clock-history-threshold
                                                               (* 3600 24))))
                                                     0)))
                      (optional " " (*? nonl)))))

(defun akirak-org-clock--recently-active-p (file &optional days)
  "Return non-nil is there is a recent activity in FILE."
  (cl-flet
      ((find-ts ()
         ;; A regular expression based on `org-ts-regexp-inactive' from org.el.
         (re-search-forward (format "\\[\\(%s\\)\\]"
                                    (akirak-org-clock--date-regxps days))
                            nil t)))
    (if-let (buffer (find-buffer-visiting file))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char (point-min))
           (find-ts)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((org-inhibit-startup t)
              (org-modules-loaded t))
          (delay-mode-hooks (org-mode)))
        (find-ts)))))

;;;; Open clocked entries

(defcustom akirak-org-clock-open-hook nil
  "Hook run after `akirak-org-clock-open' jumps to a heading."
  :type 'hook)

(defmacro akirak-org-clock-require-clock (&rest body)
  (declare (indent 0))
  `(if (org-clocking-p)
       (progn
         ,@body)
     (user-error "There is no running clock")))

;;;###autoload
(defun akirak-org-clock-open (&optional arg show-buffer-fn)
  "Open the currently clocked entry in a capture/indirect buffer.

This function returns the current buffer."
  (interactive "P")
  (akirak-org-clock-require-clock
    (if-let (capture-buffer (akirak-org-clock--capture-buffer org-clock-marker))
        (with-current-buffer capture-buffer
          (pop-to-buffer (current-buffer))
          (when arg
            (goto-char (org-entry-end-position))
            (delete-blank-lines)
            (newline))
          capture-buffer)
      (with-current-buffer (org-dog-indirect-buffer org-clock-marker)
        (when (or (< (point) (point-min))
                  (> (point) (point-max)))
          (goto-char (point-min)))
        (funcall (or show-buffer-fn #'pop-to-buffer) (current-buffer))
        (when org-dog-new-indirect-buffer-p
          (org-back-to-heading)
          (run-hooks 'akirak-org-clock-open-hook))
        (when arg
          (goto-char (org-entry-end-position))
          (delete-blank-lines)
          (newline))
        (current-buffer)))))

;;;###autoload
(defun akirak-org-clock-goto ()
  "Switch to an indirect buffer of the clocked entry."
  (interactive)
  (akirak-org-clock-open nil #'switch-to-buffer))

(defun akirak-org-clock--capture-buffer (clock-marker)
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

;;;; Other utilities

(defun akirak-org-clock-transfer-entries (dest)
  (let ((dest-logbook (with-current-buffer (marker-buffer dest)
                        (org-with-wide-buffer
                         (goto-char dest)
                         (akirak-org-clock--find-or-create-logbook)))))
    (let (entries)
      (save-excursion
        (save-restriction
          (widen)
          (org-back-to-heading)
          (narrow-to-region (point) (org-entry-end-position))
          (while (re-search-forward (rx-to-string `(and bol (* (any " \\t"))
                                                        ,org-clock-string
                                                        (+ (any " \\t"))))
                                    nil t)
            (beginning-of-line 1)
            (let ((start (point))
                  (end (line-beginning-position 2)))
              (push (buffer-substring-no-properties (point) end) entries)
              (delete-region (point) end)
              (goto-char start)))
          (goto-char (point-min))
          (replace-regexp (rx bol (* (any " \\t")) ":LOGBOOK:\n"
                              (* (any " \\t"))  ":END:\n")
                          "")))
      (with-current-buffer (marker-buffer dest-logbook)
        (org-with-wide-buffer
         (goto-char dest-logbook)
         (while entries
           (insert (pop entries)))
         (org-hide-drawer-all)))
      (org-back-to-heading))))

(defun akirak-org-clock--find-or-create-logbook ()
  "Go to the end of the log book of the entry."
  (org-back-to-heading)
  (let ((bound (org-entry-end-position)))
    (if (re-search-forward org-logbook-drawer-re bound t)
        (beginning-of-line 1)
      (forward-line)
      (if (re-search-forward org-property-drawer-re bound t)
          (insert "\n")
        (while (looking-at org-planning-line-re)
          (forward-line)))
      (insert ":LOGBOOK:\n:END:\n")
      (beginning-of-line 0)))
  (point-marker))

;;;###autoload
(defun akirak-org-clock-log (start end)
  "Insert a clock entry into the logbook."
  (interactive (let* ((start (org-read-date t t nil "Start" nil nil t))
                      (end (org-read-date t t nil "End" nil nil t)))
                 (list start end)))
  (when (org-before-first-heading-p)
    (user-error "Not on an entry"))
  (save-excursion
    (org-back-to-heading)
    (if (re-search-forward org-logbook-drawer-re (org-entry-end-position) t)
        (progn
          (goto-char (car (match-data)))
          (forward-line 1))
      (org-end-of-meta-data)
      (insert ":LOGBOOK:\n:END:\n")
      (forward-line -1))
    (let ((fmt (org-time-stamp-format t t)))
      (insert org-clock-string " "
              (org-format-time-string fmt start)
              "--"
              (org-format-time-string fmt end)
              " =>  0:00"
              "\n")
      (end-of-line 0)
      (org-clock-update-time-maybe))))

;;;###autoload
(defun akirak-org-clock-transfer-avy ()
  (interactive)
  (let ((dest (save-selected-window
                (save-excursion
                  (akirak-org-avy-heading t)
                  (point-marker)))))
    (akirak-org-clock-transfer-entries dest)))

;;;; Clock out commands

;;;###autoload
(defun akirak-org-clock-out (&optional arg)
  (interactive "P")
  (akirak-org-clock-require-clock
    (if-let (capture-buffer (akirak-org-clock--capture-buffer org-clock-marker))
        (with-current-buffer capture-buffer
          (org-capture-finalize))
      (org-clock-out arg))))

;;;###autoload
(defun akirak-org-clock-done ()
  (interactive)
  (akirak-org-clock-require-clock
    (org-with-clock-position (list org-clock-marker)
      (akirak-org-clock--finalize-capture
       (org-todo 'done)))))

;;;###autoload
(defun akirak-org-clock-set-review ()
  (interactive)
  (akirak-org-clock-require-clock
    (org-with-clock-position (list org-clock-marker)
      (akirak-org-clock--finalize-capture
       (org-schedule nil)
       ;; If you add the todo keyword to `org-clock-out-when-done', `org-clock-out'
       ;; will be tirggered when you switch to the state.
       (org-todo "REVIEW")))))

;;;; Stack

;; Currently unused.

;;;###autoload
(defun akirak-org-clock-push (marker)
  "Push a clock marker to the stack with the current window configuration."
  (push (cons marker (current-window-configuration))
        akirak-org-clock-stack))

(defun akirak-org-clock-pop (marker)
  "Pop a clock marker and its associated window configuration"
  (pcase (pop akirak-org-clock-stack)
    (`(,marker . ,wconf)
     (set-window-configuration wconf)
     (org-clock-clock-in (list marker)))))

;;;; Edit

;;;###autoload
(defun akirak-org-clock-edit-log-entry ()
  "Edit a clock entry in the logbook of the node."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "You must run this command inside org-mode"))
  (when (org-before-first-heading-p)
    (user-error "Please run this command inside an Org entry"))
  (cl-flet
      ((to-cell (elem)
         (let ((ts (thread-last
                     elem
                     (org-element-property :value))))
           (cons (org-element-property :raw-value ts)
                 ts))))
    (let* ((alist (mapcar #'to-cell (akirak-org-clock--entries)))
           (choice (if (= 1 (length alist))
                       (caar alist)
                     (completing-read "Clock: " alist nil t)))
           (ts (cdr (assoc choice alist)))
           (new-value (read-from-minibuffer "Edit clock: " choice)))
      (save-excursion
        (goto-char (org-element-property :begin ts))
        (atomic-change-group
          (delete-region (org-element-property :begin ts)
                         (org-element-property :end ts))
          (insert new-value
                  (if (rx blank) "" " "))
          (org-clock-update-time-maybe))))))

(defun akirak-org-clock--entries ()
  (let* ((bound (org-entry-end-position))
         clocks)
    (save-excursion
      (org-back-to-heading)
      (while (re-search-forward org-clock-line-re bound t)
        (push (org-element-clock-parser (pos-eol)) clocks)))
    (nreverse clocks)))

(provide 'akirak-org-clock)
;;; akirak-org-clock.el ends here
