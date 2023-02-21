;;; akirak-org-log.el ---  -*- lexical-binding: t -*-

(defcustom akirak-org-log-file nil
  ""
  :type 'file)

;; Deprecated in favor of defaulting to simple week cycles.
(defcustom akirak-org-log-weekly-body-function
  (lambda (start-date end-date)
    (concat (format "[[org-memento:timeline?span=week&date=%s,%s]]\n\n"
                    start-date end-date)
            (format (if-let (marker (akirak-org-log--find-latest-week-entry))
                        (org-with-point-at marker
                          (concat (org-link-make-string (concat "id:" (org-id-get-create))
                                                        "Last week")
                                  "\n\n"))
                      ""))
            "%?\n\n"
            (format "#+begin_src emacs-lisp
(org-memento-agenda \"%s\" \"%s\"
  :super-groups '((:auto-property \"MEMENTO_CATEGORY\")))
#+end_src" start-date end-date)))
  ""
  :type 'function)

(defcustom akirak-org-log-week-entry-hook nil
  "Hook run after `akirak-org-log-goto-week-entry' jumps to a weekly entry."
  :type 'hook)

;;;###autoload
(defun akirak-org-log-goto-week-entry (&optional arg)
  (interactive "P")
  (find-file akirak-org-log-file)
  (widen)
  (org-reverse-datetree-goto-date-in-file
   (if arg
       (org-read-date nil t (when (numberp arg)
                              (format "%dW" arg)))
     (let ((org-use-last-clock-out-time-as-effective-time nil)
           (org-use-effective-time t))
       (org-current-effective-time))))
  (beginning-of-line)
  (switch-to-buffer (org-dog-indirect-buffer))
  (run-hooks 'akirak-org-log-week-entry-hook))

;; Deprecated
(defun akirak-org-log-goto-week-entry-1 (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'akirak-org-log-insert-new-week-entry)
    (when-let (marker (akirak-org-log--find-latest-week-entry))
      (org-goto-marker-or-bmk marker)
      ;; TODO: Don't depend on org-ql-find
      (run-hooks 'org-ql-find-goto-hook))))

;; Deprecated
(defun akirak-org-log--find-latest-week-entry ()
  (with-current-buffer (or (find-buffer-visiting akirak-org-log-file)
                           (find-file-noselect akirak-org-log-file))
    (org-with-wide-buffer
     ;; To make org-complex-heading-regexp match case-sensitively
     (let ((case-fold-search nil))
       (catch 'found-entry
         (goto-char (point-min))
         (while (re-search-forward org-complex-heading-regexp nil t)
           (when (and (string-match-p (regexp-quote "Weekly PPP")
                                      (match-string 4))
                      (string-match-p (regexp-quote ":@assessment:")
                                      (match-string 5)))
             (org-back-to-heading)
             (throw 'found-entry (point-marker)))))))))

;; Deprecated
(defun akirak-org-log-insert-new-week-entry (start-date)
  (interactive (list (org-read-date nil t)))
  (let* ((end-date (thread-first
                     (decode-time start-date)
                     (decoded-time-add (make-decoded-time :day 6))
                     (encode-time)))
         (org-capture-entry
          (car (doct
                `((""
                   :keys ""
                   :file ,akirak-org-log-file
                   :function (lambda ()
                               (org-reverse-datetree-goto-date-in-file
                                ',start-date))
                   :template ,(akirak-org-capture-make-entry-body
                                (format "Weekly PPP %s [/]"
                                        (format-time-string
                                         (org-time-stamp-format nil t)
                                         start-date))
                                :deadline end-date
                                :tags '("@assessment")
                                :body (funcall akirak-org-log-weekly-body-function
                                               (format-time-string "%F" start-date)
                                               (format-time-string "%F" end-date)))
                   :clock-in t :clock-resume t))))))
    (org-capture)))

;;;###autoload
(defun akirak-org-log-generate-memento-blocks ()
  "Insert memento blocks from weekly goals."
  (cl-flet
      ((parse-time (string)
         (thread-first
           string
           (parse-time-string)
           (org-memento--set-time-of-day 0 0 0)
           (encode-time)))
       (clean-heading (string)
         (with-temp-buffer
           (insert string)
           (goto-char (point-min))
           (while (re-search-forward org-ts-regexp nil t)
             (replace-match ""))
           (goto-char (point-min))
           (when (looking-at (rx (+ space)))
             (goto-char (match-end 0)))
           (when (looking-at (rx (group (+? anything)) (?  "." (* space)) eos))
             (match-string-no-properties 1)))))
    (let* ((today (save-excursion
                    (re-search-backward (rx bol "*" blank))
                    (when (org-match-line org-complex-heading-regexp)
                      (parse-time (match-string-no-properties 4)))))
           link
           items)
      (org-with-point-at (akirak-org-log--find-latest-week-entry)
        (setq link (org-link-make-string (org-id-store-link) "Weekly goals"))
        (org-end-of-meta-data t)
        (let ((bound (org-entry-end-position)))
          (while (re-search-forward org-ts-regexp bound t)
            (when-let* ((label (save-excursion
                                 (save-match-data
                                   (org-at-item-checkbox-p)
                                   (when (equal "[ ]" (match-string 1))
                                     (goto-char (match-end 0))
                                     (buffer-substring-no-properties (point) (pos-eol))))))
                        (day (parse-time (match-string 1))))
              (when (or (time-equal-p day today)
                        (time-less-p day today))
                (push label items))))))
      (unless (bolp)
        (newline))
      (dolist (item items)
        (insert "** [#A] " (clean-heading item) "\n"
                link "\n")))))

;;;###autoload
(defun org-dblock-write:planning (_params)
  "Generate a check list of items with a planning timestamp in the range."
  (pcase-let*
      ((heading (org-get-heading t t t t))
       (today (decode-time))
       (`(,start ,end ,desc)
        (pcase heading
          ((rx bos "W" (group (+ digit)))
           (let ((weeknum (string-to-number (match-string 1 heading))))
             (org-clock-special-range
              'week (encode-time
                     (make-decoded-time :year (decoded-time-year today)
                                        :month 1 :day (* 7 weeknum)
                                        :hour 0 :minute 0 :second 0))
              'as-strings))))))
    (cl-flet
        ((format-element (element)
           (let ((done (eq 'done (org-element-property :todo-type element)))
                 (headline (org-link-display-format (org-element-property :raw-value element)))
                 (id (org-element-property :ID element)))
             (format "- [%s] %s"
                     (if done "X" " ")
                     (org-link-make-string (if id
                                               (concat "id:" id)
                                             (concat "*" headline))
                                           headline)))))
      (insert "Plans for " desc ":\n\n"
              (mapconcat #'format-element
                         (org-ql-select (current-buffer)
                           `(and (planning :to ,end)
                                 (not (closed :to ,start)))
                           :sort 'scheduled)
                         "\n")))))

(provide 'akirak-org-log)
;;; akirak-org-log.el ends here
