;;; akirak-elixir.el --- Extra functions for Elixir -*- lexical-binding: t -*-

(defcustom akirak-elixir-module-path-rewrite-rules nil
  "Alist of patterns to rewrite module paths."
  :type '(alist :key-type regexp
                :value-type string))

;;;###autoload
(defun akirak-elixir-module-name-from-file (&optional filename)
  (let ((path (mapconcat (lambda (seg)
                           (thread-last
                             (split-string seg "_")
                             (mapcar #'capitalize)
                             (string-join)))
                         (thread-last
                           (or filename (buffer-file-name))
                           (file-name-sans-extension)
                           (file-name-split)
                           (reverse)
                           (seq-take-while (lambda (seg) (not (member seg '("lib" "test")))))
                           (reverse))
                         ".")))
    (pcase-dolist (`(,regexp . ,rep) akirak-elixir-module-path-rewrite-rules)
      (setq path (replace-regexp-in-string regexp rep path)))
    path))

(provide 'akirak-elixir)
;;; akirak-elixir.el ends here
