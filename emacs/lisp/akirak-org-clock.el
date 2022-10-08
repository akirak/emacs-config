;;; akirak-org-clock.el ---  -*- lexical-binding: t -*-

(require 'org-clock)
(require 'thunk)

(defcustom akirak-org-clock-history-threshold (* 3600 24 7)
  "Number of seconds for which you want to take account for clock
 activities."
  :type 'number)

;;;; Global mode to ensure clocking

(defvar akirak-org-clock-snooze-until nil)

;;;###autoload
(define-minor-mode akirak-org-clock-mode
  "Ensure clocking"
  :global t
  :lighter " OrgClock"
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
                                  ,(expand-file-name "~/resources/articles/")))
                     (and ".gpg" eol)
                     "/.git/")))

(defconst akirak-org-clock-buffer-name-whitelist
  ;; Don't block saving buffers created using `with-temp-buffer'
  (rx bos (or " *temp*"
              "CAPTURE-")))

(defsubst akirak-org-clock--snoozed-p ()
  (and akirak-org-clock-snooze-until
       (< (float-time) akirak-org-clock-snooze-until)))

(defadvice save-buffer (around akirak-org-clock activate)
  (when (or (string-match-p akirak-org-clock-buffer-name-whitelist
                            (buffer-name))
            (string-match-p akirak-org-clock-file-name-whitelist
                            (if-let (base (buffer-base-buffer))
                                (buffer-file-name base)
                              buffer-file-name))
            (bound-and-true-p url-http-content-type)
            (and (derived-mode-p 'org-mode)
                 (or (bound-and-true-p org-capture-mode)
                     (and (featurep 'org-dog)
                          (org-dog-buffer-object))))
            (akirak-org-clock--snoozed-p)
            (akirak-org-clock--check-before-save))
    ad-do-it))

(defun akirak-org-clock--check-before-save ()
  (require 'org-clock)
  (require 'akirak-org-dog)
  (if-let (pr (project-current))
      (thunk-let* ((files0 (akirak-org-dog-project-files))
                   (files (thread-last
                            (org-dog-overview-scan files0 :fast t)
                            (mapcar #'car)))
                   (filename (thread-last
                               (marker-buffer org-clock-marker)
                               (buffer-file-name)
                               (abbreviate-file-name))))
        (if (string-match-p (regexp-quote "/foss/contributions/")
                            (project-root pr))
            (if-let (mode-file (car (akirak-org-dog-major-mode-files)))
                (or (and (org-clocking-p)
                         (equal filename mode-file)
                         (member "@contribution"
                                 (save-current-buffer
                                   (org-with-point-at org-clock-marker
                                     (org-get-tags)))))
                    (progn
                      (require 'org-dog-clock)
                      (message "You must clock into %s" mode-file)
                      (org-dog-clock-in mode-file :query-prefix "tags:@contribution "
                                        :tags "@contribution"
                                        :prompt
                                        (format "Clock in (%s): "
                                                (akirak-org-clock--project-name pr)))
                      t))
              (error "No Org file for the major mode %s" major-mode))
          (if files0
              (or (when (org-clocking-p)
                    (or (member filename files0)
                        (member filename files)))
                  (progn
                    (require 'org-dog-clock)
                    (message "You must clock in")
                    (org-dog-clock-in files :query-prefix "todo: "
                                      :prompt
                                      (format "Clock in (%s): "
                                              (akirak-org-clock--project-name pr)))
                    t))
            (user-error "No Org file for the project in %s"
                        (project-root pr)))))
    (user-error "Not in a project. First create a project")))

;;;###autoload
(defun akirak-org-clock-in-to-project ()
  "Clock in to an entry in a file related to the current project."
  (interactive)
  (if-let (pr (project-current))
      (if (string-match-p (regexp-quote "/foss/contributions/")
                          (project-root pr))
          (org-dog-clock-in (car (akirak-org-dog-major-mode-files))
                            :query-prefix "todo: tags:@contribution "
                            :prompt
                            (format "Clock in (%s): "
                                    (akirak-org-clock--project-name pr)))
        (let ((files (thread-last
                       (org-dog-overview-scan (akirak-org-dog-project-files)
                                              :fast t)
                       (mapcar #'car))))
          (org-dog-clock-in files :query-prefix "todo: "
                            :prompt
                            (format "Clock in (%s): "
                                    (akirak-org-clock--project-name pr)))))
    (user-error "No project")))

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
            (and buffer-file-name
                 ;; I sometimes edit Org file inside `user-emacs-directory', and
                 ;; I don't want to
                 (string-match-p akirak-org-clock-file-name-whitelist
                                 buffer-file-name))
            (and (bound-and-true-p org-dog-file-mode)
                 (or (org-before-first-heading-p)
                     (akirak-org-clock--snoozed-p)
                     ;; It is likely that I mistype 'y' or 'n' to skip the question,
                     ;; so require an uppercase letter.
                     (pcase (read-char-choice "Clock in to this entry? " '(?Y ?N))
                       (?Y
                        (org-clock-in)
                        (run-with-timer akirak-org-clock-reclock-interval
                                        nil #'akirak-org-clock-reclock-in)
                        t)
                       (?N
                        (org-dog-clock-in (cl-remove-duplicates
                                           (list "~/org/meta.org"
                                                 (thread-last
                                                   (org-base-buffer (current-buffer))
                                                   (buffer-file-name)
                                                   (abbreviate-file-name)))
                                           :test #'equal)
                                          :query-prefix "todo: ")
                        t)))))
    ad-do-it))

;;;###autoload
(defun akirak-org-clock-snooze ()
  (interactive)
  (setq akirak-org-clock-snooze-until
        (+ (float-time) akirak-org-clock-snooze-duration))
  (add-hook 'org-clock-in #'akiraik-org-clock-stop-snoozing))

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

(provide 'akirak-org-clock)
;;; akirak-org-clock.el ends here
