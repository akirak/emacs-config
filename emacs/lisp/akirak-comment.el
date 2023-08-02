;;; akirak-comment.el ---  -*- lexical-binding: t -*-

(require 'newcomment)
(require 'syntax)

(defcustom akirak-comment-prefer-block-comment nil
  "Whether to use block comments if available."
  :type 'boolean)

;;;###autoload
(defun akirak-comment-toggle (&optional arg)
  (interactive "P")
  ;; TODO: If inside a comment, jump to the beginning of the comment.
  (cond
   ((numberp arg)
    (comment-line arg))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (let ((ppss (syntax-ppss)))
      (if (eq 'comment (syntax-ppss-context (syntax-ppss)))
          (akirak-comment--uncomment-ppss ppss)
        (if (or (eolp) (looking-at (rx (+ blank) eol)))
            (if (looking-back (rx (* blank)) (pos-bol))
                (comment-dwim nil)
              (comment-line 1))
          (save-excursion
            (let ((start (point)))
              (when (looking-at (rx (+ blank)))
                (goto-char (match-end 0)))
              (cond
               ;; uncomment a block comment
               ((and (bound-and-true-p c-block-comment-start-regexp)
                     (looking-at c-block-comment-start-regexp))
                (delete-region (match-beginning 0) (match-end 0))
                (re-search-forward c-block-comment-ender-regexp)
                (delete-region (match-beginning 0) (match-end 0))
                (cycle-spacing 0))
               ;; uncomment line comments
               ((or (and (bound-and-true-p c-line-comment-start-regexp)
                         (looking-at c-line-comment-start-regexp))
                    (and comment-start
                         (looking-at (or comment-start-skip
                                         comment-start))))
                (if (equal arg '(4))
                    ;; With a single universal prefix, uncomment a sequence of line
                    ;; comments.
                    (let ((regexp (rx-to-string
                                   `(and (* blank)
                                         (or eol
                                             ,@(when comment-start-skip
                                                 `((regexp ,comment-start-skip)))
                                             ,@(when c-line-comment-start-regexp
                                                 `((regexp ,c-line-comment-start-regexp))))))))
                      (while (looking-at regexp)
                        (beginning-of-line 2))
                      (uncomment-region start (line-end-position 0)))
                  (comment-line 1)))
               ;; Comment the sexp.
               (t
                (unless (string-match-p (rx bol (* blank) eol)
                                        (buffer-substring
                                         (line-beginning-position) (point)))
                  (newline-and-indent))
                (let ((initial-depth (syntax-ppss-depth (syntax-ppss)))
                      (eol-depth (progn
                                   (end-of-line)
                                   (syntax-ppss-depth (syntax-ppss)))))
                  (cond
                   ((> eol-depth initial-depth)
                    (backward-up-list (- eol-depth initial-depth))
                    (forward-sexp)
                    (when (looking-at ";")
                      (goto-char (match-end 0)))
                    (unless (looking-at (rx (* blank) eol))
                      (open-line 1))
                    (comment-region start (point)))
                   ((< eol-depth initial-depth)
                    (goto-char start)
                    (forward-sexp)
                    (when (looking-at ";")
                      (goto-char (match-end 0)))
                    (unless (looking-at (rx (* blank) eol))
                      (open-line 1))
                    (comment-region start (point)))
                   (t
                    (comment-line 1))))))))))))))

(defun akirak-comment--uncomment-ppss (ppss)
  "Uncomment the current line/block according to PPSS."
  (save-excursion
    (let ((start (ppss-comment-or-string-start ppss)))
      (goto-char start)
      (if comment-use-syntax-ppss
          (progn
            (forward-comment 1)
            (uncomment-region start (point)))
        (error "Non-ppss is currently unsupported")))))

;;;###autoload
(defun akirak-comment-region-1 (begin end &optional arg)
  "Comment a region."
  (let ((block-comment-start (or block-comment-start
                                 (bound-and-true-p c-block-comment-starter)))
        (block-comment-end (or block-comment-end
                               (bound-and-true-p c-block-comment-ender))))
    (if (and akirak-comment-prefer-block-comment
             block-comment-start
             block-comment-end
             (save-excursion
               (goto-char begin)
               (not (search-forward block-comment-start end t))))
        (progn
          (goto-char end)
          (let ((end-marker (point-marker)))
            (goto-char begin)
            (let ((prefix (buffer-substring (line-beginning-position) (point))))
              (open-line 1)
              (insert block-comment-start)
              (when (string-match-p (rx bol (+ blank) eol) prefix)
                (beginning-of-line 2)
                (insert prefix)))
            (goto-char end-marker)
            (newline-and-indent)
            (insert block-comment-end)))
      (comment-region-default begin end arg))))

(provide 'akirak-comment)
;;; akirak-comment.el ends here
