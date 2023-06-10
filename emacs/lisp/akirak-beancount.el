;;; akirak-beancount.el ---  -*- lexical-binding: t -*-

(require 'beancount)

(defgroup akirak-beancount nil
  ""
  :group 'beancount)

(defvar akirak-beancount-last-date nil)

(defvar akirak-beancount-last-account nil)

(defvar-local akirak-beancount-highlighted-regexp nil)

(defun akirak-beancount-highlight-regexp (regexp)
  (when akirak-beancount-highlighted-regexp
    (unhighlight-regexp akirak-beancount-highlighted-regexp))
  (highlight-regexp regexp)
  (setq akirak-beancount-highlighted-regexp regexp))

(defun akirak-beancount-unhighlight ()
  "Clear the current highlights."
  (interactive nil beancount-mode)
  (when akirak-beancount-highlighted-regexp
    (unhighlight-regexp akirak-beancount-highlighted-regexp)
    (setq akirak-beancount-highlighted-regexp nil)))

;;;###autoload
(defun akirak-beancount-locate-account-transactions ()
  (interactive nil beancount-mode)
  (let* ((width (frame-width))
         (sections (thread-last
                     (akirak-beancount--scan-sections)
                     (mapcar (lambda (cell)
                               (cons (car cell)
                                     (org-format-outline-path (cdr cell) width))))))
         (sections1 (copy-sequence sections))
         (accounts (akirak-beancount--scan-open-accounts))
         (current-section (cdr (pop sections1)))
         (account-section-map (make-hash-table :test #'equal :size (length accounts))))
    (pcase-dolist (`(,account . ,pos) accounts)
      (puthash account
               (if (and sections1 (> pos (caar sections1)))
                   (setq current-section (cdr (pop sections1)))
                 current-section)
               account-section-map))
    (cl-labels
        ((group-function (account transform)
           (if transform
               account
             (gethash account account-section-map)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'beancount-account)
                           (cons 'group-function #'group-function)))
             (complete-with-action action (mapcar #'car accounts)
                                   string pred))))
      (let ((selection (completing-read "Select or create an account: "
                                        #'completions)))
        (if (assoc selection accounts)
            (let ((regexp (regexp-quote selection)))
              (save-window-excursion
                (occur regexp))
              (akirak-beancount-highlight-regexp regexp)
              (goto-char (point-min))
              (next-error)
              (recenter))
          (let* ((account-alist2 (akirak-beancount--similar-accounts selection accounts))
                 (sections2 (thread-last
                              account-alist2
                              (mapcar (lambda (cell)
                                        (gethash (car cell) account-section-map)))
                              (seq-uniq))))
            (if (and (= 1 (length sections2))
                     (yes-or-no-p (format "Add a new account to %s? "
                                          (org-no-properties (car sections2)))))
                (progn
                  (goto-char (cdr (car (last account-alist2))))
                  (forward-line))
              (goto-char (car (rassoc (completing-read "Section: " (mapcar #'cdr sections)
                                                       nil t)
                                      sections)))
              (forward-line))
            (akirak-beancount--account-list-end)
            (open-line)
            (insert (org-read-date) " open " selection)))))))

(defun akirak-beancount--account-list-end ()
  "Find the last position of an open directive in the current section."
  (let ((start (point)))
    (or (re-search-forward beancount-outline-regexp nil t)
        (goto-char (point-max)))
    (if (catch 'found-open-directive
          (while (re-search-backward beancount-timestamped-directive-regexp start t)
            (when (equal (match-string 2) "open")
              (throw 'found-open-directive t))))
        (beginning-of-line 2)
      (goto-char start))))

(defun akirak-beancount--similar-accounts (account account-alist)
  (cl-flet ((parent (account)
              (string-join (butlast (split-string account ":")) ":")))
    (catch 'result
      (while (not (string-empty-p account))
        (setq account (parent account))
        (when-let (alist (cl-member-if `(lambda (cell)
                                          (or (equal ,account (car cell))
                                              (string-prefix-p ,account (car cell))))
                                       account-alist))
          (throw 'result (seq-take-while `(lambda (cell)
                                            (string-prefix-p ,account (car cell)))
                                         alist)))))))

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

(defun akirak-beancount--scan-sections ()
  (let (result
        olp-reversed
        last-level)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^" beancount-outline-regexp "[[:blank:]]+")
                                nil t)
        (let ((pos (match-beginning 0))
              (level (- (match-end 0) (match-beginning 0)))
              (heading (buffer-substring (point) (line-end-position))))
          (setq olp-reversed
                (if (or (not last-level)
                        (> level last-level))
                    (cons heading olp-reversed)
                  (cons heading (seq-drop olp-reversed (1+ (- last-level level))))))
          (push (cons pos (reverse olp-reversed))
                result)
          (setq last-level level))))
    (nreverse result)))

;;;###autoload
(defun akirak-beancount-insert-date ()
  (interactive)
  (let ((org-read-date-prefer-future nil))
    (insert (org-read-date nil nil nil nil nil akirak-beancount-last-date))))

;;;###autoload
(defun akirak-beancount-rename-account (old-account new-account)
  "Rename an account in the ledger."
  (interactive (let* ((accounts (akirak-beancount--scan-open-accounts))
                      (old-account (completing-read "Select an account to rename: "
                                                    accounts
                                                    nil t)))
                 (list old-account
                       (completing-read "New name of the account: "
                                        accounts
                                        nil nil old-account)))
               beancount-mode)
  (when (member new-account (akirak-beancount--scan-open-accounts))
    (user-error "Don't select an existing name: \"%s\"" new-account))
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (rx-to-string `(and word-start ,old-account word-end))))
      (while (re-search-forward regexp nil t)
        (replace-match new-account)))))

(provide 'akirak-beancount)
;;; akirak-beancount.el ends here
