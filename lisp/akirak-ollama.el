;;; akirak-ollama.el --- Ollama support -*- lexical-binding: t -*-

(defun akirak-ollama-list ()
  "Return the list of LLM model names available on the machine."
  (with-temp-buffer
    (unless (zerop (call-process "ollama" nil (list t nil) nil
                                 "list"))
      (error "ollama list returned non-zero"))
    (goto-char (point-min))
    (let (result)
      (while (< (point) (point-max))
        (forward-line)
        (when (looking-at (rx bol (+ (not (any blank)))))
          (push (match-string 0) result)))
      result)))

(provide 'akirak-ollama)
;;; akirak-ollama.el ends here
