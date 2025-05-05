;;; akirak-org-clock.el ---  -*- lexical-binding: t -*-

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

;;; Code:


(require 'org-clock)
(require 'thunk)

(defvar akirak-emacs-org-config-file)

(defcustom akirak-org-clock-history-threshold (* 3600 24 2)
  "Number of seconds for which you want to take account for clock
 activities.

Example values are shown below:

 * 2 days = 172800 seconds
 * 3 days = 259200 seconds
 * 5 days = 432000 seconds
 * 7 days = 604800 seconds
 * 10 days = 864000 seconds"
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

(defcustom akirak-org-clock-pre-exit-hook nil
  "Hook to run before `org-clock-out' triggered by the user."
  :type 'hook)

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
                                  ,(expand-file-name "~/archives/")
                                  ,(expand-file-name "~/resources/articles/")
                                  (and ,(expand-file-name "~/") (any upper))
                                  "/tmp"))
                     (and (or "emacs-config.org"
                              "-log.org"
                              "/org/config.el")
                          eol)
                     "/private-config/"
                     "/.")))

(defconst akirak-org-clock-buffer-name-whitelist
  ;; Don't block saving certain buffers created using `with-temp-buffer'
  (rx bos (or " *temp*"
              "CAPTURE-")))

(defsubst akirak-org-clock--snoozed-p ()
  (and akirak-org-clock-snooze-until
       (< (float-time) akirak-org-clock-snooze-until)))

(defadvice save-buffer (around akirak-org-clock activate)
  (require 'akirak-emacs-org)
  (when-let* ((filename (if-let* ((base (buffer-base-buffer)))
                            (buffer-file-name base)
                          buffer-file-name)))
    (when (let ((case-fold-search nil))
            (or (not (memq this-command '(save-buffer
                                          compile
                                          project-compile
                                          recompile
                                          magit-status)))
                (string-match-p akirak-org-clock-buffer-name-whitelist
                                (buffer-name))
                (string-match-p akirak-org-clock-file-name-whitelist
                                filename)
                (not (string-prefix-p "~/" (abbreviate-file-name filename)))
                (and (bound-and-true-p akirak-emacs-org-config-file)
                     (string-equal (expand-file-name akirak-emacs-org-config-file)
                                   filename))
                (bound-and-true-p url-http-content-type)
                ;; (memq this-command '(magit-show-commit
                ;;                      magit-status
                ;;                      bookmark-set))
                (when-let* ((mode (derived-mode-p 'org-mode 'org-memento-policy-mode)))
                  (cl-case mode
                    (org-memento-policy-mode t)
                    (org-mode (or (bound-and-true-p org-capture-mode)
                                  (and (featurep 'org-dog)
                                       (org-dog-buffer-object))))))
                (akirak-org-clock--snoozed-p)
                (file-remote-p filename)
                (akirak-org-clock--check-before-save)))
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
                (or (not (string-prefix-p "~/" filename))
                    (member filename files)
                    (when further
                      (member filename
                              (thread-last
                                (org-dog-overview-scan files :fast t)
                                (mapcar #'car))))
                    (member filename (akirak-org-clock--mode-or-path-files))))
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
           t)))
    (`nil
     t)))

(defun akirak-org-clock--mode-or-path-files ()
  (cl-remove-duplicates (append (akirak-org-dog-major-mode-files)
                                (akirak-org-dog-path-files))
                        :test #'equal))

(defun akirak-org-clock--target ()
  (or akirak-org-clock-target
      (pcase (or (vc-git-root default-directory)
                 (cond
                  ((not (string-prefix-p "~/" (abbreviate-file-name default-directory)))
                   nil)
                  ((yes-or-no-p "Not in a project. Run git init?")
                   (when (buffer-file-name)
                     (let ((dir (file-name-directory (buffer-file-name))))
                       (unless (file-directory-p dir)
                         (make-directory dir 'parents))))
                   (let ((default-directory (read-directory-name
                                             "Run git init at: ")))
                     (call-process "git" nil nil nil "init")
                     default-directory))
                  (t
                   (user-error "Must be in a project"))))
        (`nil)
        ((rx "/foss/contributions/")
         (list (delq nil (list (car (akirak-org-dog-path-files))
                               (car (akirak-org-dog-major-mode-files))))
               "tag:@contribution "
               "@contribution"
               nil))
        ((and (rx "/learning/" (group (+ (not (any "/")))) "/")
              (app (match-string 1) category))
         (list (org-dog-select 'absolute
                 `(relative :regexp ,(rx-to-string `(and "/" ,category
                                                         (?  "." (+ (not (any "/"))))
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
         (require 'akirak-org-dog)
         (list (append (akirak-org-dog-project-files)
                       (akirak-org-dog-path-files)
                       (akirak-org-dog-major-mode-files))
               "todo: "
               nil
               nil)))))

;;;###autoload
(defun akirak-org-clock-in-to-project ()
  "Clock in to an entry in a file related to the current project."
  (interactive)
  (require 'akirak-org-dog)
  (pcase-exhaustive (akirak-org-clock--target)
    (`nil
     (user-error "You are not in a project or the project is not suitable for\
 clocking in" ))
    (`(,files ,query-prefix ,tags ,further)
     (if-let* ((files (if further
                          (thread-last
                            (org-dog-overview-scan files
                                                   :fast t)
                            (mapcar #'car))
                        files)))
         (org-dog-clock-in files
                           :query-prefix query-prefix
                           :tags tags
                           :prompt
                           (format "Clock in (files: %s): "
                                   (mapconcat #'file-name-nondirectory
                                              files ", ")))
       (message "No default clock target, so fall back to octopus-clock-in")
       (octopus-clock-in)))))

(defun akirak-org-clock--project-name (pr)
  "Return the name of the project for use in prompt."
  (thread-last
    (if (eq (car pr) 'vc)
        (vc-git-root (project-root pr))
      (project-root pr))
    (string-remove-suffix "/")
    (file-name-nondirectory)))

(defcustom akirak-org-clock-snooze-duration 60
  "Duration in seconds of snoozing in Org mode."
  :type 'number)

(defcustom akirak-org-clock-reclock-interval 20
  "Reclocking interval."
  :type 'number)

(defadvice org-self-insert-command (around akirak-org-clock activate)
  (if (akirak-org-clock--org-allow-p)
      ad-do-it
    (user-error "Please clock in first")))

(defadvice org-insert-heading (around akirak-org-clock activate)
  (if (or (akirak-org-clock--org-allow-p)
          (not (called-interactively-p 'any)))
      ad-do-it
    (or (unless (and (memq this-command '(org-meta-return
                                          org-insert-todo-heading))
                     (looking-at (rx nonl)))
          (akirak-capture-org-ins-heading-fallback current-prefix-arg))
        ad-do-it)))

(defun akirak-org-clock--org-allow-p ()
  (or (org-clocking-p)
      (bound-and-true-p org-capture-mode)
      (let ((filename (buffer-file-name (org-base-buffer (current-buffer)))))
        ;; Pass non-file buffers like *Org Note* buffers.
        (or (not filename)
            (bound-and-true-p org-memento-file-mode)
            ;; I sometimes edit Org file inside `user-emacs-directory', and
            ;; I don't want to
            (string-match-p akirak-org-clock-file-name-whitelist
                            filename)
            (not (bound-and-true-p org-dog-file-mode))
            (org-before-first-heading-p)
            (akirak-org-clock--snoozed-p)))))

;;;###autoload
(defun akirak-org-clock-snooze (&optional seconds)
  (interactive "P")
  (when akirak-org-clock-mode
    (let ((seconds (or (when (numberp seconds)
                         seconds)
                       akirak-org-clock-snooze-duration)))
      (setq akirak-org-clock-snooze-until
            (+ (float-time) seconds))
      (message "Snoozing org clock mode for %s seconds" seconds)
      (add-hook 'org-clock-in-hook #'akirak-org-clock-stop-snoozing))))

(defun akirak-org-clock-stop-snoozing ()
  (setq akirak-org-clock-snooze-until nil))

(defun akirak-org-clock-reclock-in ()
  "Reclock in for updating the title."
  (when (org-clocking-p)
    (save-current-buffer
      (org-with-point-at org-clock-marker
        (org-clock-in)))))

;;;; Persist the context to the clocked entry

;;;###autoload
(defun akirak-org-clock-add-git-properties (&optional arg)
  "Add Git properties to the entry at POM if it has none."
  (interactive "P")
  (if (org-clocking-p)
      (pcase arg
        ('(16)
         (akirak-org-clock-locate-git-file))
        ('(4)
         (akirak-org-git-add-properties-if-none (with-current-buffer (akirak-org-clock-open)
                                                  (point-marker))
                                                'force))
        (_
         (akirak-org-git-add-properties-if-none org-clock-marker 'force)))
    (user-error "You have to be running a clock")))

;;;###autoload
(defun akirak-org-clock-locate-git-file ()
  (interactive nil)
  (akirak-org-git-locate-source org-clock-marker))

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
  "Return non-nil is there is any recent activity in FILE."
  (let ((regexp (format "\\[\\(%s\\)\\]"
                        (akirak-org-clock--date-regxps days))))
    (cl-flet
        ((find-ts ()
           ;; A regular expression based on `org-ts-regexp-inactive' from org.el.
           (re-search-forward regexp nil t)))
      (if-let* ((buffer (find-buffer-visiting file)))
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
          (find-ts))))))

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
    (let ((action '(nil . ((inhibit-same-window . t)))))
      (if-let* ((capture-buffer (akirak-org-clock--capture-buffer org-clock-marker)))
          (with-current-buffer capture-buffer
            (unless (get-buffer-window capture-buffer)
              (pop-to-buffer capture-buffer action))
            (when arg
              (goto-char (org-entry-end-position))
              (delete-blank-lines)
              (newline))
            capture-buffer)
        (with-current-buffer (org-dog-indirect-buffer org-clock-marker)
          (unless (get-buffer-window (current-buffer))
            (funcall (or show-buffer-fn #'pop-to-buffer) (current-buffer)
                     action))
          (cond
           ((or arg
                (org-match-line org-clock-line-re)
                (org-match-line org-logbook-drawer-re))
            (goto-char (org-entry-end-position))
            (delete-blank-lines)
            (newline))
           ;; I don't know if this path actually happens.
           ((or (< (point) (point-min))
                (> (point) (point-max)))
            (goto-char (point-min))))
          (when org-dog-new-indirect-buffer-p
            (run-hooks 'akirak-org-clock-open-hook))
          (current-buffer))))))

(defun akirak-org-clock-buffer ()
  (or (akirak-org-clock--capture-buffer org-clock-marker)
      (org-dog-indirect-buffer org-clock-marker)))

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
(defun akirak-org-clock-display-commit-entry ()
  "Display an Org entry that refers to the commit."
  (interactive)
  (pcase (akirak-org-clock-find-commit-entry)
    ((and (map :marker)
          (guard marker))
     (message "Found an entry")
     (with-current-buffer (org-dog-indirect-buffer marker)
       (funcall #'display-buffer (current-buffer)
                '(nil . ((inhibit-same-window . t))))
       (org-back-to-heading)
       (run-hooks 'akirak-org-clock-open-hook)))
    ((and (map :multi)
          (guard multi))
     (user-error "Found multiple matches"))
    (_
     (user-error "No entry found"))))

;;;###autoload
(defun akirak-org-clock-reclock-commit-entry ()
  "If a commit entry is found, `org-clock-in` to it."
  (interactive)
  (pcase (akirak-org-clock-find-commit-entry)
    ((and (map :marker)
          (guard marker))
     (org-clock-clock-in (list marker)))
    ((and (map :multi)
          (guard multi))
     (user-error "Found multiple matches"))
    (_
     (user-error "No entry found"))))

(defun akirak-org-clock-find-commit-entry ()
  "Return a plist containing information of the Git commit at the current line."
  (unless (buffer-file-name)
    (user-error "Not visiting a file"))
  (unless (vc-git-root (buffer-file-name))
    (user-error "Not inside a Git repository"))
  (pcase-let*
      ((filename (file-name-nondirectory (buffer-file-name)))
       (line-number (save-restriction
                      (widen)
                      (line-number-at-pos)))
       (`(,rev ,message) (with-temp-buffer
                           (unless (zerop (call-process "git" nil (list t nil) nil
                                                        "--no-pager"
                                                        "blame"
                                                        "-L" (format "%d,%d"
                                                                     line-number
                                                                     line-number)
                                                        "--porcelain"
                                                        "--" filename))
                             (error "Git blame failed"))
                           (list (progn
                                   (goto-char (point-min))
                                   (looking-at (rx (+ hex)))
                                   (match-string-no-properties 0))
                                 (progn
                                   (re-search-forward (rx bol "summary "))
                                   (buffer-substring-no-properties
                                    (point) (line-end-position))))))
       (`(,files1 ,_query-prefix ,_ ,further) (akirak-org-clock--target))
       (files2 (if further
                   (thread-last
                     (org-dog-overview-scan files1 :fast t)
                     (mapcar #'car))
                 files1)))
    (pcase (or (catch 'exact-match
                 (org-ql-select files2
                   `(regexp ,(rx-to-string `(and "[[orgit-rev:" (+ nonl) "::"
                                                 ,rev "][" (+ nonl) "]]")))
                   :action `(throw 'exact-match (list (point-marker)))))
               (org-ql-select files2
                 `(regexp ,(rx-to-string `(and bol (regexp ,org-ts-regexp-inactive)
                                               (+ blank) ,message)))
                 :action '(point-marker)))
      (`(,marker)
       (list :marker marker :rev rev :summary message))
      (`nil
       (list :rev rev :summary message))
      (markers
       (list :multi t :markers markers :rev rev :summary message)))))

;;;###autoload
(defun akirak-org-clock-transfer-avy ()
  (interactive)
  (let ((dest (save-selected-window
                (save-excursion
                  (akirak-org-avy-heading t)
                  (point-marker)))))
    (akirak-org-clock-transfer-entries dest)))

;;;###autoload
(defun akirak-org-clock-in (&optional arg)
  "A custom variant of `org-clock-in'."
  (interactive "P")
  (if (equal arg '(4))
      (let ((org-clock-in-switch-to-state nil))
        (org-clock-in))
    (org-clock-in)))

;;;; Clock out commands

;;;###autoload
(defun akirak-org-clock-out (&optional arg)
  (interactive "P")
  (akirak-org-clock-require-clock
    (pcase arg
      ('(16)
       (let ((current-name (tab-bar-tab-name-current)))
         (tab-bar-rename-tab org-clock-heading)
         (tab-bar-duplicate-tab)
         (akirak-org-clock--out)))
      ('(4)
       (akirak-org-clock--out t))
      (_
       (akirak-org-clock--out)))))

(defun akirak-org-clock--out (&optional switch-state)
  (if-let* ((capture-buffer (akirak-org-clock--capture-buffer org-clock-marker)))
      (let ((need-explicit-clock-out (and (org-clocking-p)
                                          (not (equal org-clock-marker
                                                      (buffer-local-value
                                                       'org-capture-clock-was-started
                                                       capture-buffer))))))
        (run-hooks 'akirak-org-clock-pre-exit-hook)
        (when need-explicit-clock-out
          (org-clock-out switch-state))
        (with-current-buffer capture-buffer
          (org-capture-finalize)))
    (run-hooks 'akirak-org-clock-pre-exit-hook)
    (org-clock-out switch-state)))

;;;###autoload
(defun akirak-org-clock-done (&optional arg)
  (interactive)
  (akirak-org-clock-require-clock
    (org-with-clock-position (list org-clock-marker)
      (akirak-org-clock--finalize-capture
       (let ((kwd (if arg
                      (or (org-fast-todo-selection)
                          ;; If SPC is selected inside org-fast-todo-selection,
                          ;; nil will be returned, but it should be an empty
                          ;; string when passed to org-todo.
                          "")
                    'done)))
         (run-hooks 'akirak-org-clock-pre-exit-hook)
         (org-todo kwd))))))

;;;###autoload
(defun akirak-org-clock-set-review ()
  (interactive)
  (akirak-org-clock-require-clock
    (org-with-clock-position (list org-clock-marker)
      (akirak-org-clock--finalize-capture
       (run-hooks 'akirak-org-clock-pre-exit-hook)
       (org-schedule nil)
       ;; If you add the todo keyword to `org-clock-out-when-done', `org-clock-out'
       ;; will be tirggered when you switch to the state.
       (org-todo "REVIEW")))))

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

;;;; Log references

(defun akirak-org-clock-log-reference-url (url)
  "Log URL into the references drawer."
  (unless (org-clocking-p)
    (error "Not clocking in"))
  (org-with-point-at (or org-clock-hd-marker
                         (error "org-clock-hd-marker is not set"))
    (catch 'already-exists
      (if (re-search-forward (rx bol (* blank) ":REFERENCES:" (or blank eol))
                             (org-entry-end-position) t)
          (if (re-search-forward (rx-to-string `(and bol ,url eol))
                                 ;; The bound is set to the end of the entry
                                 ;; instead of the metadata block. Practically,
                                 ;; this would be fine.
                                 (org-entry-end-position)
                                 t)
              (throw 'already-exists t)
            (re-search-forward "^[ \t]*:END:[ \t]*$")
            (beginning-of-line)
            (org-open-line 1))
        (org-end-of-meta-data t)
        (re-search-backward (rx nonl eol))
        (beginning-of-line 2)
        (insert ":REFERENCES:\n:END:\n")
        (beginning-of-line 0)
        (org-open-line 1))
      (insert url))))

(provide 'akirak-org-clock)
;;; akirak-org-clock.el ends here
