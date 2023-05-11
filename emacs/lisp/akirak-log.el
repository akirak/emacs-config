;;; akirak-log.el ---  -*- lexical-binding: t -*-

(defcustom akirak-log-private-file (locate-user-emacs-file "log/latest.org")
  ""
  :type 'file)

;;;###autoload
(define-minor-mode akirak-log-mode
  "Global minor mode that logs certain events to an Org file."
  :global t
  (when akirak-log-mode
    (let ((dir (file-name-directory akirak-log-private-file)))
      (unless (file-directory-p dir)
        (make-directory dir t))))
  (funcall (if akirak-log-mode
               #'add-hook
             #'remove-hook)
           'find-file-hook #'akirak-log--find-file)
  (funcall (if akirak-log-mode
               #'add-hook
             #'remove-hook)
           'magit-status-mode-hook #'akirak-log--project))

(defun akirak-log--enabled-p ()
  (not (and (fboundp 'org-clocking-p)
            (org-clocking-p))))

(cl-defun akirak-log--heading (heading &key time tags message)
  (with-current-buffer (or (org-find-base-buffer-visiting akirak-log-private-file)
                           (find-file-noselect akirak-log-private-file))
    (org-with-wide-buffer
     (goto-char (point-max))
     (insert (format-spec "* %t %h%g
:PROPERTIES:
:emacs_pid:  %p
:login:      %l
:exact_time: %u
:END:\n"
                          `((?t . ,(format-time-string
                                    (org-time-stamp-format t t)
                                    time))
                            (?u . ,(format-time-string "%FT%H:%M:%S%:z"))
                            (?h . ,heading)
                            (?g . ,(if tags
                                       (concat " "
                                               (org-make-tag-string
                                                (ensure-list tags)))
                                     ""))
                            (?p . ,(emacs-pid))
                            (?l . ,(concat (user-login-name) "@" (system-name)))))
             (when message
               (string-chop-newline message)))
     (when message
       (insert-char ?\n)))))

(defun akirak-log--format-file-name (file)
  ;; TODO: Prevent loading ol for a shorter initial response time?
  (require 'ol)
  (org-link-make-string
   (concat "file:" (abbreviate-file-name file))))

(defun akirak-log--find-file ()
  (when (and (or (memq current-minibuffer-command '(consult-recent-file
                                                    consult-buffer))
                 (not (file-exists-p (buffer-file-name))))
             (akirak-log--enabled-p))
    (akirak-log--heading (akirak-log--format-file-name (buffer-file-name))
                         :tags "file"
                         :message "Visit a recent file.")))

(defun akirak-log--project ()
  ;; None of `current-minibuffer-command', `this-command' and its alternatives
  ;; can detect invocation of `akirak-consult-dir' or `akirak-project-switch',
  ;; so use (not (eq this-command 'magit-status)) here.
  (when (and (not (eq this-command 'magit-status))
             (akirak-log--enabled-p))
    (akirak-log--heading (akirak-log--format-file-name default-directory)
                         :tags "project"
                         :message "Switch to a project.")))

;;;###autoload
(defun akirak-log-memento-activities (start-time end-time)
  (when (file-readable-p akirak-log-private-file)
    (let (result
          (start-time-float (float-time start-time))
          (end-time-float (float-time end-time)))
      (with-current-buffer (or (find-buffer-visiting akirak-log-private-file)
                               (find-file-noselect akirak-log-private-file))
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward (rx bol "* ") nil t)
           (when (looking-at org-ts-regexp-inactive)
             (let ((time (thread-last
                           (save-match-data
                             (if-let (string (save-match-data
                                               (org-entry-get nil "exact_time")))
                                 (parse-iso8601-time-string string)
                               (encode-time (parse-time-string (match-string 1)))))
                           (float-time))))
               (when (and (>= time start-time-float)
                          (< time end-time-float))
                 (goto-char (match-end 0))
                 (if (org-match-line org-complex-heading-regexp)
                     (let* ((rem-headline (buffer-substring-no-properties
                                           (point) (match-end 4)))
                            (link (when (string-match org-link-bracket-re
                                                      rem-headline)
                                    (match-string 1 rem-headline))))
                       (push (list time time
                                   (or link rem-headline)
                                   (copy-marker (line-beginning-position))
                                   'event)
                             result))
                   (error "Failed to match on line: %s" (point-marker)))))))))
      result)))

(provide 'akirak-log)
;;; akirak-log.el ends here
