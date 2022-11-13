;;; akirak-org-log.el ---  -*- lexical-binding: t -*-

(defcustom akirak-org-log-file nil
  ""
  :type 'file)

(defcustom akirak-org-log-weekly-body-function
  (lambda (start-date end-date)
    (concat (format "[[org-memento:timeline?span=week&date=%s,%s]]\n\n"
                    start-date end-date)
            "%?"))
  ""
  :type 'function)

;;;###autoload
(defun akirak-org-log-goto-week-entry (&optional arg)
  (interactive "P")
  (if arg
      (call-interactively #'akirak-org-log-insert-new-week-entry)
    (find-file akirak-org-log-file)
    (let ((initial-pos (point))
          ;; To make org-complex-heading-regexp match case-sensitively
          (case-fold-search nil))
      (catch 'found-entry
        (goto-char (point-min))
        (while (re-search-forward org-complex-heading-regexp nil t)
          (when (and (string-match-p (regexp-quote "Weekly PPP")
                                     (match-string 4))
                     (string-match-p (regexp-quote ":@assessment:")
                                     (match-string 5)))
            (org-back-to-heading)
            (require 'org-ql-find)
            ;; TODO: Don't depend on org-ql-find
            (run-hooks 'org-ql-find-goto-hook)
            (throw 'found-entry t)))
        (goto-char initial-pos)))))

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
