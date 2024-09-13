;;; akirak-paren.el --- Parenthesis -*- lexical-binding: t -*-

(defcustom akirak-paren-match-hook nil
  "Hook that returns a matching location.

It should return a point that is a counterpart of the current
location, or nil."
  :type 'hook)

;;;###autoload
(defun akirak-paren-goto-match-or-self-insert (n &optional c)
  (interactive "p")
  (if-let (loc (and (not (ppss-comment-or-string-start (syntax-ppss)))
                    (akirak-paren-matching-location)))
      (goto-char loc)
    (self-insert-command n c)))

(defun akirak-paren-matching-location ()
  (pcase (funcall show-paren-data-function)
    (`(,hb ,he ,tb ,te nil)
     (cond
      ((= hb (point))
       te)
      ((= he (point))
       tb)
      ((= tb (point))
       he)
      ((= te (point))
       hb)
      (t
       (run-hook-with-args-until-success 'akirak-paren-match-hook))))
    (_
     (run-hook-with-args-until-success 'akirak-paren-match-hook))))

(defun akirak-paren-syntax-table-match ()
  "A syntax-based matching function for `akirak-paren-match-hook’."
  (let ((syn (syntax-after (point))))
    (pcase (syntax-class syn)
      ;; open parenthesis
      (4
       (save-excursion
         (forward-sexp)
         (point)))
      ;; close parenthesis
      (5
       ;; TODO: Support delimiters that consists of more than one characters
       (1+ (car (last (ppss-open-parens (syntax-ppss))))))
      (_
       (let ((syn2 (syntax-after (1- (point)))))
         (pcase (syntax-class syn2)
           (4
            ;; There may be a more efficient implementation
            (save-excursion
              (backward-up-list)
              (forward-sexp)
              (backward-char)
              (point)))
           (5
            (save-excursion
              (backward-sexp)
              (point)))))))))

(defvar akirak-paren-jump-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "%" #'akirak-paren-goto-match-or-self-insert)
    map))

;;;###autoload
(define-minor-mode akirak-paren-jump-mode
  "")

;;;###autoload
(define-global-minor-mode akirak-paren-jump-global-mode
  akirak-paren-jump-mode akirak-paren-jump-mode)

;;;###autoload
(defun akirak-paren-delete (c)
  "Delete a pair of parentheses/brackets of C around the point."
  (interactive "cDelete a bracket pair opening with: ")
  (save-excursion
    (let* ((regexp (regexp-quote (char-to-string c)))
           (start (if (looking-at regexp)
                      (point)
                    (re-search-backward regexp)))
           (end (akirak-paren-matching-location)))
      (save-excursion
        (goto-char start)
        (delete-char 1)
        (goto-char (1- end))
        (backward-delete-char 1)))))

;;;###autoload
(defun akirak-paren-replace (c)
  "Replace a pair of parentheses/brackets around the point."
  (interactive "cReplace a bracket pair opening with: ")
  (save-excursion
    (let* ((regexp (regexp-quote (char-to-string c)))
           (start (if (looking-at regexp)
                      (point)
                    (re-search-backward regexp)))
           (end (akirak-paren-matching-location))
           (overlay (make-overlay start end))
           (replacement-char (progn
                               (overlay-put overlay 'face 'highlight)
                               (read-char "New paren: ")))
           (replacement-close-char (akirak-paren--close-char replacement-char)))
      (save-excursion
        (delete-overlay overlay)
        (goto-char start)
        (delete-char 1)
        (insert-char replacement-char)
        (goto-char end)
        (backward-delete-char 1)
        (insert-char replacement-close-char)))))

(defun akirak-paren--close-char (open-char)
  "Return a character corresponding to OPEN-CHAR.

Also see `akirak-elec-pair--close-char'."
  (or (nth 1 (electric-pair-syntax-info open-char))
      (matching-paren open-char)
      open-char))

;;;###autoload
(defun akirak-paren-select-inner (c)
  "Select the inner text inside a pair of parentheses/brackets."
  (interactive "cSelect text inside a bracket pair opening with: ")
  (deactivate-mark)
  (let* ((regexp (regexp-quote (char-to-string c)))
         (start (if (looking-at regexp)
                    (point)
                  (re-search-backward regexp)))
         (end (akirak-paren-matching-location)))
    (goto-char (1+ start))
    (push-mark)
    (goto-char (1- end))
    (activate-mark)))

;;;###autoload
(defun akirak-paren-select-outer (c)
  "Select the outer text inside a pair of parentheses/brackets."
  (interactive "cSelect text outside a bracket pair opening with: ")
  (deactivate-mark)
  (let ((regexp (regexp-quote (char-to-string c)))
        )
    (if (looking-at regexp)
        (point)
      (re-search-backward regexp))
    (push-mark)
    (goto-char (akirak-paren-matching-location))
    (activate-mark)))

(provide 'akirak-paren)
;;; akirak-paren.el ends here
