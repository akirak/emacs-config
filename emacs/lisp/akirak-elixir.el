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

;;;###autoload
(defun akirak-elixir-replace-module-name ()
  "Replace the current module name with the default one."
  (interactive)
  (let ((new-name (akirak-elixir-module-name-from-file)))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (re-search-forward (rx bol "defmodule" (+ blank)
                                      (group (+ (any "." alnum)))
                                      ;; Only support do...end, as the inline syntax is
                                      ;; the least likely to be used for modules
                                      (+ blank) "do")
                                  nil t)
          (let ((orig-name (match-string 1)))
            (replace-match new-name t nil nil 1)
            (message "Replaced %s with %s" orig-name new-name)))))))

(provide 'akirak-elixir)
;;; akirak-elixir.el ends here
