;;; akirak-org-clock.el ---  -*- lexical-binding: t -*-

(require 'org-clock)

(defcustom akirak-org-clock-history-threshold (* 3600 24 7)
  "Number of seconds for which you want to take account for clock
 activities."
  :type 'number)

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
