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

(provide 'akirak-org-log)
;;; akirak-org-log.el ends here
