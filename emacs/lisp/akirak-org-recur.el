;;; akirak-org-recur.el --- org-recur support -*- lexical-binding: t -*-

(require 'org-recur)

(defcustom akirak-org-recur-options
  '("+2 / recur every other day"
    "+w / recur every week"
    "1 / recur on the first day of every month"
    "Thu / recur every Thursday"
    "Sun,Sat / recur every Sunday and Saturday"
    "Wkdy / recur every weekday"
    "1 10:00, 15 12:00 / recur at particular time on particular date")
  ""
  :type '(repeat string))

(defcustom akirak-org-recur-finish-hook
  '(akirak-org-recur-revert-done-subtasks
    org-clock-out)
  ""
  :type 'hook)

(defmacro akirak-org-recur--with-agenda-entry (&rest progn)
  `(when-let ((marker (or (org-get-at-bol 'org-marker)
                          (org-get-at-bol 'org-hd-marker))))
     (with-current-buffer (marker-buffer marker)
       (goto-char marker)
       ,@progn)))

(defun akirak-org-recur-next-date ()
  "Return non-nil if the current heading has a recurring state."
  (org-recur--get-next-date (org-get-heading t t t t)))

(defun akirak-org-recur-todo ()
  "A replacement for `org-todo' in `org-recur' enabled buffers."
  (interactive)
  (if (akirak-org-recur-next-date)
      (cl-ecase (read-char-choice "Choose an action [d: done, e: edit schedule]: "
                                  (string-to-list "de"))
        (?d (org-recur-finish))
        (?e (akirak-org-recur-set)))
    (call-interactively #'org-todo)))

(defun akirak-org-recur-agenda-todo ()
  "A replacement for `org-agenda-todo' with `org-recur' support."
  (interactive)
  (akirak-org-recur--with-agenda-entry
   (akirak-org-recur-todo)))

(defun akirak-org-recur-set ()
  "Set a recurring schedule."
  (interactive)
  (let* ((date-desc (completing-read "Schedule: " akirak-org-recur-options))
         (date (if (string-match (rx bol (group (+? anything)) "/") date-desc)
                   (string-trim-right (match-string 1 date-desc))
                 date-desc))
         (cookie (format "|%s|" date))
         (orig-headline (org-get-heading t t t t))
         (headline (concat cookie " "
                           (if (string-match (concat "^" "\\(" org-recur--regexp "\\)")
                                             orig-headline)
                               (substring orig-headline (1+ (match-end 1)))
                             orig-headline))))
    (org-edit-headline headline)))

(defun akirak-org-recur-maybe-setup ()
  "If the state is recurring, set a schedule."
  (when (equal (org-get-todo-state) "RECUR")
    (unless (bound-and-true-p org-recur-mode)
      (when (yes-or-no-p "org-recur-mode is inactive. Turn on it?")
        (org-recur-mode 1)
        (when (yes-or-no-p "Add to the file header?")
          (save-excursion
            (add-file-local-variable-prop-line 'mode 'org)
            (add-file-local-variable-prop-line 'mode 'org-recur)))))
    (unless (akirak-org-recur-next-date)
      (akirak-org-recur-set))
    (unless (org-entry-get nil "SCHEDULED")
      (org-recur-schedule-date (akirak-org-recur-next-date)))))

(defun akirak-org-recur-revert-done-subtasks (&rest _args)
  (save-excursion
    (let ((pos (point))
          (end (save-excursion (org-end-of-subtree))))
      (org-map-entries
       (lambda ()
         (when (and (org-entry-is-done-p)
                    (member "@recur" (org-get-tags)))
           (org-todo 'none)))
       nil 'tree))))

(defun akirak-org-recur-run-finish-hook ()
  (run-hooks 'akirak-org-recur-finish-hook))

;;;###autoload
(define-minor-mode akirak-org-recur-mode
  "Turn on the opinionated integration with org-recur."
  :global t
  (if akirak-org-recur-mode
      (progn
        (advice-add 'org-recur-finish :after #'akirak-org-recur-run-finish-hook)
        (add-hook 'org-after-todo-state-change-hook #'akirak-org-recur-maybe-setup)
        (define-key org-recur-mode-map [remap org-todo] #'akirak-org-recur-todo)
        (define-key org-agenda-mode-map
                    [remap org-agenda-todo] #'akirak-org-recur-agenda-todo))
    (progn
      (advice-remove 'org-recur-finish #'akirak-org-recur-run-finish-hook)
      (remove-hook 'org-after-todo-state-change-hook #'akirak-org-recur-maybe-setup)
      (define-key org-recur-mode-map [remap org-todo] nil)
      (define-key org-agenda-mode-map [remap org-agenda-todo] nil))))

(provide 'akirak-org-recur)
;;; akirak-org-recur.el ends here
