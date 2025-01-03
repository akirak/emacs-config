;;; akirak-worktree.el --- Custom support for working trees -*- lexical-binding: t -*-

(defcustom akirak-worktree-directories nil
  "List of directories inside which the worktree mode is turned on."
  :type '(repeat directory))

;;;###autoload
(define-minor-mode akirak-worktree-file-mode
  "Minor mode for files inside working trees."
  :lighter nil)

;;;###autoload
(define-minor-mode akirak-worktree-mode
  "Minor mode for working trees.

This minor mode was created merely for the purpose of providing extra
directory-local settings. By enabling `akirak-worktree-global-mode' and
adding hooks to `akirak-worktree-file-mode' (file buffers) or
`akirak-worktree-mode', you can run functions only inside certain
working trees specified in `akirak-worktree-directories'."
  :lighter nil
  (when (buffer-file-name)
    (akirak-worktree-file-mode)))

;;;###autoload
(defun akirak-worktree-mode-enable ()
  (interactive)
  (unless (or (minibufferp)
              akirak-worktree-mode)
    (let ((cwd (abbreviate-file-name default-directory)))
      (when (cl-find-if `(lambda (dir) (string-prefix-p dir ,cwd))
                        akirak-worktree-directories)
        (akirak-worktree-mode 1)))))

;;;###autoload
(define-globalized-minor-mode akirak-worktree-global-mode
  akirak-worktree-mode akirak-worktree-mode-enable)

(provide 'akirak-worktree)
;;; akirak-worktree.el ends here
