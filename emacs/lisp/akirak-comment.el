;;; akirak-comment.el ---  -*- lexical-binding: t -*-

(require 'newcomment)
(require 'syntax)

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
                    (comment-region start (if (looking-at ";")
                                              (match-end 0)
                                            (point))))
                   ((< eol-depth initial-depth)
                    (goto-char start)
                    (forward-sexp)
                    (comment-region start (if (looking-at ";")
                                              (match-end 0)
                                            (point))))
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
  (pcase (akirak-comment--multi-line-info begin end)
    (`(,block-comemnt-start ,block-comment-end ,indentation)
     (let ((end-marker (progn
                         (goto-char end)
                         (point-marker))))
       (goto-char begin)
       (unless (looking-at (rx (* blank) eol))
         (open-line 1))
       (delete-region (line-beginning-position) (point))
       (insert (make-string indentation ?\s) block-comment-start)
       (forward-line)
       (let ((content-start (point)))
         (goto-char end-marker)
         (if (looking-back (rx bol (* blank)) (line-beginning-position))
             (delete-region (match-beginning 0) (point))
           (newline))
         ;; (replace-regexp-in-region (rx bol)
         ;;                           (make-string (1+ (length block-comment-start)) ?\s)
         ;;                           content-start (line-end-position 0))
         (insert (make-string indentation ?\s) block-comment-end)
         (unless (looking-at (rx (* blank) eol))
           (newline-and-indent)))))
    (_
     (comment-region-default begin end arg))))

(defun akirak-comment--multi-line-info (begin end)
  (when-let* ((block-comment-start (akirak-comment--block-start-syntax))
              (block-comment-end (akirak-comment--block-end-syntax)))
    (save-excursion
      (goto-char begin)
      (when (and (> end (line-end-position))
                 (not (search-forward block-comment-start end t)))
        (while (looking-at (rx (* blank) eol))
          (beginning-of-line 2))
        (list block-comment-start
              block-comment-end
              (current-indentation))))))

(defun akirak-comment--block-start-syntax ()
  (or block-comment-start
      (bound-and-true-p c-block-comment-starter)
      comment-start))

(defun akirak-comment--block-end-syntax ()
  (or block-comment-end
      (bound-and-true-p c-block-comment-ender)
      (unless (string-empty-p comment-end)
        comment-end)))

;;;###autoload
(defun akirak-uncomment-region-1 (begin end &optional arg)
  (uncomment-region-default-1 begin end arg)
  (save-excursion
    (goto-char begin)
    (delete-matching-lines (rx bol (* blank) eol)
                           (line-beginning-position)
                           (line-end-position))))

(provide 'akirak-comment)
;;; akirak-comment.el ends here
