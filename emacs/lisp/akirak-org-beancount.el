;;; akirak-org-beancount.el ---  -*- lexical-binding: t -*-

(require 'org)
(require 'akirak-beancount)

(defgroup akirak-org-beancount nil
  ""
  :group 'akirak-beancount
  :group 'akirak-org)

(defcustom akirak-org-beancount-account-property "BEANCOUNT_ACCOUNT"
  ""
  :type 'string)

(defcustom akirak-org-beancount-recipients-property "ITEM_AVAILABLE_AT"
  ""
  :type 'string)

(cl-defstruct akirak-org-beancount-receipt-context
  date recipient payment)

(defvar akirak-org-beancount-receipt-context nil)

(defvar akirak-org-beancount-recipients nil)

(defvar akirak-org-beancount-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode akirak-org-beancount-mode
  "Minor mode for tracking items in `org-mode'."
  :init-value nil
  (if akirak-org-beancount-mode
      (add-hook 'before-save-hook #'akirak-org-beancount-sort-tables
                nil t)
    (remove-hook 'before-save-hook #'akirak-org-beancount-sort-tables
                 t)))

;;;###autoload
(defun akirak-org-beancount-start-receipt ()
  (interactive)
  (setq akirak-org-beancount-receipt-context
        (make-akirak-org-beancount-receipt-context
         :date (org-read-date)
         :recipient (completing-read "Recipient: " (akirak-org-beancount-recipients))
         :payment (completing-read "Payment: " (akirak-beancount-account-completions)
                                   nil t "Assets:"))))

(defun akirak-org-beancount-recipients ()
  (or akirak-org-beancount-recipients
      (setq akirak-org-beancount-recipients
            (cl-remove-duplicates
             (apply #'append
                    (org-map-entries
                     (lambda ()
                       (org-entry-get-multivalued-property
                        nil akirak-org-beancount-recipients-property))))
             :test #'equal))))

;;;###autoload
(defun akirak-org-beancount-log-expense (&optional arg)
  (interactive "P")
  (when arg
    (akirak-org-beancount-start-receipt))
  (let* ((account (akirak-org-beancount-assign-account))
         (payment (akirak-org-beancount-receipt-context-payment
                   akirak-org-beancount-receipt-context))
         (recipient (akirak-org-beancount-receipt-context-recipient
                     akirak-org-beancount-receipt-context))
         (date (akirak-org-beancount-receipt-context-date
                akirak-org-beancount-receipt-context))
         (price (read-string "Price: " (akirak-org-beancount--get-latest-price)))
         (price-c (save-match-data
                    (if (string-match (rx (group (* digit) (?  "." (+ digit)))
                                          (group (?  (+ space) (+ alnum))))
                                      price)
                        (list (match-string 1 price)
                              (match-string 2 price))
                      (error "Invalid price format"))))
         (price-num (car price-c))
         (price-currency (unless (string-empty-p (nth 1 price-c))
                           (nth 1 price-c)))
         (quantity (read-number "Quantity: " 1))
         (title (format "%s%s @ %s"
                        (if (= quantity 1)
                            ""
                          (concat (number-to-string quantity) "x "))
                        (org-get-heading t t t t)
                        recipient)))
    (akirak-beancount-add-simple-transaction :account account
                                             :date date
                                             :title title
                                             :quantity quantity
                                             :price-num price-num
                                             :price-currency price-currency
                                             :payment payment)
    (org-back-to-heading)
    (if (re-search-forward org-table-line-regexp (org-entry-end-position) t)
        (goto-char (org-table-end))
      (goto-char (org-entry-end-position))
      (insert "\n| Date | Price | Quantity | Where |\n|--+--+--+--|\n"))
    (insert "| " (org-format-time-string
                  (org-time-stamp-format nil t)
                  (org-read-date nil t date))
            " | " price-num (or price-currency
                                (concat " " akirak-beancount-currency))
            " | " (number-to-string quantity)
            " | " recipient
            " |\n")
    (unless (member recipient
                    (org-entry-get-multivalued-property
                     nil akirak-org-beancount-recipients-property))
      (org-entry-add-to-multivalued-property
       nil akirak-org-beancount-recipients-property recipient))))

;;;###autoload
(defun akirak-org-beancount-assign-account ()
  (interactive)
  (or (org-entry-get nil akirak-org-beancount-account-property t)
      (if-let (account (completing-read "Select an account for the item: "
                                        (akirak-beancount-account-completions)))
          (progn
            (org-entry-put nil akirak-org-beancount-account-property account)
            account)
        (user-error "You need an account"))))

(defun akirak-org-beancount--get-latest-price ()
  (let (price
        (end (org-entry-end-position)))
    (save-excursion
      (org-back-to-heading)
      (while (re-search-forward
              (rx-to-string `(and "|" (+ blank)
                                  ,(akirak-org-beancount-receipt-context-recipient
                                    akirak-org-beancount-receipt-context)
                                  (+ blank) "|" (* blank) eol))
              end
              t)
        (setq price (string-trim (org-table-get-field 2)))
        (end-of-line)))
    price))

;;;###autoload
(defun akirak-org-beancount-sort-tables ()
  "Sort table rows in the buffer by date."
  (interactive)
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward org-table-line-regexp nil t)
     ;; To not corrupt the visibility state, only work on visible tables.
     (unless (org-invisible-p)
       (forward-line 2)
       (org-table-goto-column 1)
       (org-table-sort-lines nil ?a)
       (org-table-align)
       (goto-char (org-table-end))))))

(provide 'akirak-org-beancount)
;;; akirak-org-beancount.el ends here
