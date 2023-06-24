;;; akirak-beancount.el ---  -*- lexical-binding: t -*-

(require 'beancount)

(defgroup akirak-beancount nil
  ""
  :group 'beancount)

(defvar akirak-beancount-last-date nil)

(defvar akirak-beancount-last-account nil)

(defvar-local akirak-beancount-highlighted-regexp nil)

(defvar-local akirak-beancount-query nil)

(defvar-local akirak-beancount-query-buffer nil)
(defvar-local akirak-beancount-query-source nil)

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
  (when (equal old-account new-account)
    (user-error "Same account name"))
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (rx-to-string `(and word-start ,old-account word-end))))
      (while (re-search-forward regexp nil t)
        (replace-match new-account)))))

;;;###autoload
(defun akirak-beancount-balance (account)
  (interactive (list (completing-read "New name of the account: "
                                      (akirak-beancount--scan-open-accounts)
                                      nil t)))
  (goto-char (point-min))
  (if-let (pos (cdr (assoc account (akirak-beancount--scan-open-accounts))))
      (let ((date (org-read-date)))
        (goto-char pos)
        (if (re-search-forward (concat "^" beancount-outline-regexp) nil t)
            (end-of-line 0)
          (goto-char (point-max)))
        (newline)
        (insert date " balance " account " "
                (read-from-minibuffer "Enter the current balance: ")))
    (user-error "Account not found in the buffer: %s" account)))

;;;###autoload
(defun akirak-beancount-query-account (account)
  (interactive (list (completing-read "Balance "
                                      (akirak-beancount--scan-open-accounts)
                                      nil t)))
  (akirak-beancount-query
   ;; TODO: Allow customization of the query via transient
   (format "select date, narration, position, balance \
where account = \"%s\" \
order by date;" account)))

(defun akirak-beancount-query (query)
  "Dispatch bean-query command from within a journal."
  (let ((buffer (if (and akirak-beancount-query-buffer
                         (buffer-live-p akirak-beancount-query-buffer))
                    akirak-beancount-query-buffer
                  (setq akirak-beancount-query-buffer
                        (generate-new-buffer "*bean-query*"))))
        (file (buffer-file-name)))
    (add-hook 'after-save-hook #'akirak-beancount-query-rerun nil t)
    (with-current-buffer buffer
      (setq akirak-beancount-query query)
      (setq akirak-beancount-query-source file)
      (setq-local revert-buffer-function #'akirak-beancount--rerun-query)
      (read-only-mode t)
      (revert-buffer)
      (display-buffer buffer))))

(defun akirak-beancount-query-rerun ()
  (when (and akirak-beancount-query-buffer
             (buffer-live-p akirak-beancount-query-buffer))
    (with-current-buffer akirak-beancount-query-buffer
      (revert-buffer))))

(defun akirak-beancount--rerun-query (&rest _)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (call-process "bean-query" nil (list t nil) nil
                  akirak-beancount-query-source
                  akirak-beancount-query)))

(provide 'akirak-beancount)
;;; akirak-beancount.el ends here
