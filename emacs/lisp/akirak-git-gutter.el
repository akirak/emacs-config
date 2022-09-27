;;; akirak-git-gutter.el ---  -*- lexical-binding: t -*-

;;;###autoload (autoload 'akirak-git-gutter-map "akirak-git-gutter" nil 'interactive)
(define-prefix-command 'akirak-git-gutter-map)

;; I don't want the original commands bound on other maps to repeat.
(defalias 'akirak-git-gutter-next-hunk #'git-gutter:next-hunk)
(defalias 'akirak-git-gutter-previous-hunk #'git-gutter:previous-hunk)

(pcase-dolist (`(,key ,command)
               '(("n" akirak-git-gutter-next-hunk)
                 ("p" akirak-git-gutter-previous-hunk)
                 ("s" git-gutter:stage-hunk)
                 ("-" git-gutter:revert-hunk)
                 ("o" git-gutter:popup-hunk)
                 ("g" git-gutter)))
  (define-key akirak-git-gutter-map key command)
  (put command 'repeat-map 'akirak-git-gutter-map))

(define-key akirak-git-gutter-map "q" #'akirak-git-gutter-close)

(defun akirak-git-gutter-close ()
  "Close the pop-up window of git-gutter."
  (interactive)
  (when-let (window (git-gutter:popup-buffer-window))
    (quit-window t window)))

(provide 'akirak-git-gutter)
;;; akirak-git-gutter.el ends here
