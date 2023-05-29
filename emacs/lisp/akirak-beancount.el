;;; akirak-beancount.el ---  -*- lexical-binding: t -*-

(require 'beancount)

(defgroup akirak-beancount nil
  ""
  :group 'beancount)

(defvar akirak-beancount-last-date nil)

(defvar akirak-beancount-last-account nil)

;;;###autoload
(defun akirak-beancount-add-transaction ()
  "Insert a transaction into the current buffer."
  (interactive nil beancount-mode)
  (let* ((accounts (akirak-beancount--scan-open-accounts))
         (account (completing-read "Insert a transaction on an account: "
                                   accounts nil nil nil
                                   akirak-beancount-last-account)))
    (if-let (cell (assoc account accounts))
        (akirak-beancount--insert-transaction account (cdr cell))
      (when-let (pos (cdr (akirak-beancount--common-ancestor account accounts)))
        (goto-char pos)
        (while (looking-at beancount-timestamped-directive-regexp)
          (forward-line 1))
        (open-line 1)
        (insert " open " account)
        (goto-char (line-beginning-position))
        (user-error "First open an account %s" account)))))

(defun akirak-beancount--common-ancestor (account account-alist)
  (cl-flet ((parent (account)
              (string-join (butlast (split-string account ":")) ":")))
    (catch 'result
      (while (not (string-empty-p account))
        (setq account (parent account))
        (when-let (ca (cl-member-if `(lambda (cell)
                                       (or (equal ,account (car cell))
                                           (string-prefix-p ,account (car cell))))
                                    account-alist))
          (throw 'result (car ca)))))))

(defun akirak-beancount--insert-transaction (account pos)
  (goto-char pos)
  (if-let* ((bound (save-excursion
                     (if (re-search-forward (concat "^" beancount-outline-regexp) nil t)
                         (line-beginning-position)
                       (point-max))))
            (titles (akirak-beancount--scan-transactions account bound)))
      (let* ((org-read-date-prefer-future nil)
             (date (org-read-date nil nil nil "Date: " nil akirak-beancount-last-date))
             (title (completing-read "Title: " titles nil nil nil nil nil 'inherit)))
        (goto-char bound)
        (open-line 1)
        (setq akirak-beancount-last-date date
              akirak-beancount-last-account account)
        (skeleton-insert `(> ,date " * \"" ,title "\""
                             n ,account "  " _)))
    (re-search-forward (concat "^[[:blank:]]+" (regexp-quote account)) nil t)))

(defun akirak-beancount--scan-transactions (account bound)
  "Scan transactions on ACCOUNT until BOUND."
  (let (result)
    (cl-flet
        ((unquote (string)
           (if (string-match (rx bol "\"" (group (+ anything)) "\"" eol) string)
               (match-string 1 string)
             string)))
      (while (re-search-forward beancount-transaction-regexp bound t)
        (let ((title (unquote (match-string-no-properties 3))))
          (forward-line)
          (catch 'match-account
            (while (looking-at beancount-posting-regexp)
              (when (equal (match-string 1) account)
                (push (cons title (point)) result)
                (throw 'match-account t))
              (forward-line))))))
    result))

(defun akirak-beancount--scan-open-accounts ()
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward beancount-timestamped-directive-regexp nil t)
        (when (equal (match-string 2) "open")
          (push (cons (string-trim (buffer-substring-no-properties
                                    (match-end 0) (line-end-position)))
                      (line-beginning-position))
                result))))
    (nreverse result)))

;;;###autoload
(defun akirak-beancount-insert-date ()
  (interactive)
  (insert (org-read-date nil nil nil nil nil akirak-beancount-last-date)))

(provide 'akirak-beancount)
;;; akirak-beancount.el ends here
