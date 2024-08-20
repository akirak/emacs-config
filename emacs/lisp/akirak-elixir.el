;;; akirak-elixir.el --- Extra functions for Elixir -*- lexical-binding: t -*-

(defcustom akirak-elixir-module-path-rewrite-rules
  '(("\\.Live\\." . "."))
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

;;;###autoload
(defun akirak-elixir-heex-close-tag ()
  "Close the tag in heex.

This is meant to be a more reliable replacement for `sgml-close-tag'. It
can also work both in `heex-ts-mode' and inside a sigil template in
`elixir-ts-mode'."
  (interactive nil (elixir-ts-mode heex-ts-mode))
  (let ((start (point)))
    (unless (looking-at "<")
      (re-search-backward "<"))
    (while (not (member (treesit-node-type (treesit-node-at (point)))
                        '("<" "</")))
      (re-search-backward "<"))
    (let ((node (treesit-node-at (point)))
          name
          indent-level)
      (while (not (and (member (treesit-node-type node) '("tag" "component"))
                       (> (treesit-node-end node) start)))
        (setq node (treesit-node-parent node)))
      (catch 'tag-name
        (dolist (child (treesit-node-children (car (treesit-node-children node))))
          (when (member (treesit-node-type child) '("tag_name" "component_name"))
            (setq name (treesit-node-text child))
            (save-excursion
              (goto-char (treesit-node-start node))
              (when (looking-back (rx bol (* blank)) (line-beginning-position))
                (setq indent-level (- (match-end 0) (match-beginning 0)))))
            (throw 'tag-name t))))
      (goto-char start)
      (when name
        (when indent-level
          (if (looking-back (rx bol (* blank)) (line-beginning-position))
              (beginning-of-line)
            (newline))
          (indent-to indent-level))
        (insert "</" name ">")
        (delete-trailing-whitespace)))))

(provide 'akirak-elixir)
;;; akirak-elixir.el ends here
