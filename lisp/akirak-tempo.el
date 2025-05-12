;;; akirak-tempo.el --- Tempo -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-tempo-convert-to-template (begin end)
  "Convert the selected region to a tempo template."
  (interactive "r")
  (let ((source (buffer-substring begin end)))
    (delete-region begin end)
    (insert (akirak-tempo-from-string source))))

(defun akirak-tempo-from-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (replace-string-in-region "\"" "\\\"" (point-min) (point-max))
    (replace-regexp-in-region (rx bol (+ blank) eol) "" (point-min) (point-max))
    (replace-regexp-in-region (rx (+ space) eol) "" (point-min) (point-max))
    (let (all-symbols)
      (while (re-search-forward (rx symbol-start (+? nonl) symbol-end) nil t)
        (push (match-string-no-properties 0) all-symbols))
      (goto-char (point-min))
      (let* ((words (completing-read-multiple "Parameterised words: "
                                              all-symbols))
             (regexp (regexp-opt-group words))
             (indent-unit (akirak-tempo--guess-indent-unit))
             base-indent
             defined-words)
        (insert "(> ")
        (cl-flet
            ((replace-the-match (close-quote)
               (let ((str (match-string 0)))
                 (delete-region (match-beginning 0)
                                (match-end 0))
                 (insert (when close-quote
                           "\" ")
                         (if (member str defined-words)
                             (format "(s %s)"
                                     (string-inflection-kebab-case-function str))
                           (progn
                             (push str defined-words)
                             (format "(P \"%s\" %s)"
                                     str
                                     (string-inflection-kebab-case-function str))))
                         (unless (eolp)
                           " \"")))))
          (catch 'finish
            (while (< (point) (point-max))
              (unless base-indent
                (if (looking-at (rx (* blank)))
                    (setq base-indent (- (match-end 0) (match-beginning 0)))
                  (setq base-indent nil)))
              (unless (eolp)
                (if (looking-at regexp)
                    (replace-the-match nil)
                  (unless (eolp)
                    (insert-char ?\")))
                (while (re-search-forward regexp (line-end-position) t)
                  (replace-the-match t))
                (end-of-line)
                (insert-char ?\"))
              (when (> (forward-line) 0)
                (throw 'finish t))
              (if (and indent-unit
                       (looking-at (rx (+ blank))))
                  (let ((indent (- (match-end 0)
                                   (match-beginning 0))))
                    (delete-region (point) (match-end 0))
                    (insert "n ")
                    (when (> indent base-indent)
                      (dotimes (_ (/ (- indent base-indent)
                                     indent-unit))
                        (insert "> "))))
                (insert "n "))))
          (insert ")"))))
    (lisp-data-mode)
    (indent-region (point-min) (point-max))
    (ignore-errors
      (goto-char (point-min))
      (forward-symbol 1)
      (let ((pos (point)))
        (while t
          (forward-sexp)
          (when (> (- (point) (line-beginning-position)) 80)
            (backward-sexp)
            (newline-and-indent))
          (setq pos (point)))))
    (buffer-string)))

(defun akirak-tempo--guess-indent-unit ()
  (save-excursion
    (let (indents)
      (while (re-search-forward (rx bol (+ blank)) nil t)
        (push (- (match-end 0)
                 (match-beginning 0))
              indents))
      (delete-dups indents)
      (cl-sort indents #'<)
      (pcase indents
        (`(,i ,j . ,_)
         (- j i))))))

(provide 'akirak-tempo)
;;; akirak-tempo.el ends here
