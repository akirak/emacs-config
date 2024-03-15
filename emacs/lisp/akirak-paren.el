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

(provide 'akirak-paren)
;;; akirak-paren.el ends here
