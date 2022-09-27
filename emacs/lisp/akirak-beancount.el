;;; akirak-beancount.el ---  -*- lexical-binding: t -*-

(require 'beancount)

(defgroup akirak-beancount nil
  ""
  :group 'beancount)

(defcustom akirak-beancount-journal-file nil
  "The master journal file."
  :type 'file)

(defcustom akirak-beancount-currency nil
  "Default currency."
  :type 'string)

(defcustom akirak-beancount-template-alist nil
  ""
  :type '(alist :key-type string
                :value-type sexp))

(defmacro akirak-beancount--with-wide-buffer (&rest progn)
  `(with-current-buffer (or (find-buffer-visiting akirak-beancount-journal-file)
                            (find-file-noselect akirak-beancount-journal-file))
     (org-with-wide-buffer
      ,@progn)))

(defun akirak-beancount-complete-outline (prompt)
  "Jump to an outline heading."
  (goto-char (point-min))
  (let (candidates
        olp)
    (while (re-search-forward (concat "^" beancount-outline-regexp "[[:blank:]]*")
                              nil t)
      (let ((level (beancount-outline-level)))
        (setq olp (cons (string-trim-left (buffer-substring (point) (line-end-position)))
                        (seq-drop (copy-sequence olp)
                                  (- (length olp) (1- level)))))
        (push (cons (string-join (reverse olp) "/")
                    (point))
              candidates)))
    (let ((input (completing-read prompt candidates)))
      (if-let (cell (assoc input candidates))
          (progn
            (goto-char (cdr cell))
            (cdr cell))
        (let* ((ancestor (thread-last
                           candidates
                           (seq-filter (pcase-lambda (`(,path-string . ,_))
                                         (string-prefix-p path-string input)))
                           (seq-sort-by (pcase-lambda (`(,path-string . ,_))
                                          (length path-string))
                                        #'>)
                           (car)))
               (rest (thread-first
                       (car ancestor)
                       (string-remove-prefix input)
                       (split-string "/")
                       (cdr)))
               (level (1+ (length (split-string (car ancestor) "/"))))
               node)
          (goto-char (cdr ancestor))
          (outline-end-of-subtree)
          (while (setq node (pop rest))
            (insert "\n" (make-string level ?\*) " " node)
            (cl-incf level))
          (insert "\n"))
        (point)))))

(cl-defun akirak-beancount-add-simple-transaction (&key account date title quantity
                                                        price-num price-currency
                                                        payment)
  (akirak-beancount--with-wide-buffer
   (akirak-beancount--search-account account)
   (insert "\n" date " * " (format "\"%s\"" title)
           "\n  " account "  "
           (if (= quantity 1)
               price-num
             (number-to-string (* quantity (string-to-number price-num))))
           (or price-currency
               (concat " " akirak-beancount-currency))
           "\n  " payment "\n")
   (beancount-indent-transaction)))

(defun akirak-beancount-add-transaction (account)
  (interactive (list (akirak-beancount-read-account "Select an account: ")))
  (akirak-beancount--search-account account)
  (when-let (template (cdr (assoc account akirak-beancount-template-alist)))
    (insert "\n")
    (skeleton-insert template)))

(defun akirak-beancount--search-account (account)
  (goto-char (point-min))
  (or (re-search-forward (concat (rx bol (* blank)) (regexp-quote account))
                         nil t)
      (akirak-beancount-complete-outline
       (format "Where to add the transaction \"%s\": " account)))
  (when (outline-next-heading)
    (forward-line -1)))

(defun akirak-beancount-read-account (prompt)
  (let* ((accounts (akirak-beancount-accounts))
         (input (completing-read prompt accounts)))
    (unless (member input accounts)
      (if (akirak-beancount-complete-outline "Open a new account under a heading: ")
          (let ((date (org-read-date)))
            (insert date " open " input "\n"))
        (user-error "Aborted")))
    input))

(defun akirak-beancount-accounts ()
  "Return a list of accounts in the file."
  (akirak-beancount--with-wide-buffer
   (beancount-collect beancount-account-regexp 0)))

(defalias 'akirak-beancount-account-completions #'akirak-beancount-accounts
  "Return a completion table for accounts.

For now, it simply returns a list of accounts.")

(provide 'akirak-beancount)
;;; akirak-beancount.el ends here
