;;; akirak-ai-model.el --- AI models -*- lexical-binding: t -*-

(defcustom akirak-ai-model-list-file
  (locate-user-emacs-file "ai-model-list.txt")
  "Path to file that contains a list of models."
  :type 'file)

;;;###autoload
(defun akirak-ai-model-list ()
  (with-temp-buffer
    (insert-file-contents (if (file-readable-p akirak-ai-model-list-file)
                              akirak-ai-model-list-file
                            (error "File set as `akirak-ai-model-list-file' is not readable: %s"
                                   akirak-ai-model-list-file)))
    (thread-last
      (string-split (buffer-string) "\n")
      (cl-remove-if #'string-empty-p)
      (mapcar #'akirak-ai-model--normalize-name))))

(defun akirak-ai-model--normalize-name (name)
  "Convert the NAME of a model to the provider/model format."
  (save-match-data
    (pcase name
      ((rx "/")
       ;; Already normalized
       name)
      ((rx bol (or "gpt-"
                   "codex-"
                   (and "o" (any digit))))
       ;; OpenAI (gpt-5, o3, etc.)
       (concat "openai/" name))
      (_
       ;; Fallback
       name))))

;;;###autoload
(defun akirak-ai-model-prefixed-list (prefix &optional predicate)
  (declare (indent 1))
  (with-temp-buffer
    (insert-file-contents akirak-ai-model-list-file)
    (thread-last
      (string-split (buffer-string) "\n")
      (mapcar #'akirak-ai-model--normalize-name)
      (seq-filter (apply-partially #'string-prefix-p prefix))
      (mapcar (apply-partially #'string-remove-prefix prefix))
      (seq-filter (or predicate #'identity))
      (mapcar #'intern))))

(provide 'akirak-ai-model)
;;; akirak-ai-model.el ends here
