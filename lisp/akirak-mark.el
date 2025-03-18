;;; akirak-mark.el --- Mark a thing at point -*- lexical-binding: t -*-

(defmacro akirak-mark-def-thing-at-point (thing)
  `(defun ,(intern (format "akirak-mark-%s-at-point" thing)) ()
     (interactive)
     (pcase (bounds-of-thing-at-point ',thing)
       (`(,begin . ,end)
        (goto-char begin)
        (push-mark)
        (goto-char end)
        (activate-mark)))))

(defalias 'akirak-mark-thing-at-point #'akirak-mark-thing-transient)

(akirak-mark-def-thing-at-point symbol)
(akirak-mark-def-thing-at-point list)
(akirak-mark-def-thing-at-point sexp)
(akirak-mark-def-thing-at-point defun)
(akirak-mark-def-thing-at-point filename)
(akirak-mark-def-thing-at-point url)
(akirak-mark-def-thing-at-point email)
(akirak-mark-def-thing-at-point uuid)
(akirak-mark-def-thing-at-point word)
(akirak-mark-def-thing-at-point sentence)
(akirak-mark-def-thing-at-point whitespace)
(akirak-mark-def-thing-at-point line)
(akirak-mark-def-thing-at-point number)

(defun akirak-mark-button-at-point ()
  (interactive)
  (let ((end (next-single-property-change (point) 'button)))
    (goto-char (previous-single-property-change (point) 'button))
    (push-mark)
    (goto-char end)
    (activate-mark)))

(defun akirak-mark--in-text-p ()
  (or (derived-mode-p 'text-mode)
      (memq (syntax-ppss-context (syntax-ppss))
            '(string comment))))

(defun akirak-mark--at-button-p ()
  (get-char-property (point) 'button))

;;;###autoload (autoload 'akirak-mark-thing-transient "akirak-mark" nil 'interactive)
(transient-define-prefix akirak-mark-thing-transient ()
  ["Natural language"
   :class transient-row
   :if akirak-mark--in-text-p
   ("w" "word" akirak-mark-word-at-point)
   ("s" "sentence" akirak-mark-sentence-at-point)
   ("n" "number" akirak-mark-number-at-point)]
  ["Code"
   :class transient-row
   ("d" "defun" akirak-mark-defun-at-point)
   ("e" "sexp" akirak-mark-sexp-at-point)
   ("l" "line" akirak-mark-line-at-point)
   ("m" "symbol" akirak-mark-symbol-at-point)
   ("u" "url" akirak-mark-url-at-point)]
  ["Properties"
   ("b" "button" akirak-mark-button-at-point
    :if akirak-mark--at-button-p)]
  (interactive)
  (transient-setup 'akirak-mark-thing-transient))

(provide 'akirak-mark)
;;; akirak-mark.el ends here
