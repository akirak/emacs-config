;;; akirak-org-log.el ---  -*- lexical-binding: t -*-

(defcustom akirak-org-log-file nil
  ""
  :type 'file)

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

;;;###autoload
(defun akirak-org-log-goto-week-entry (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'akirak-org-log-insert-new-week-entry)
    (when-let (marker (akirak-org-log--find-latest-week-entry))
      (org-goto-marker-or-bmk marker)
      ;; TODO: Don't depend on org-ql-find
      (run-hooks 'org-ql-find-goto-hook))))

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

(provide 'akirak-org-log)
;;; akirak-org-log.el ends here
