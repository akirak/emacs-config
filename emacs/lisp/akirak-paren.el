;;; akirak-paren.el --- Parenthesis -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-paren-goto-match-or-self-insert (n &optional c)
  (interactive "p")
  (if-let (loc (akirak-paren-matching-location))
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
       hb)))))

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