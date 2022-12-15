;;; akirak-git-commit.el ---  -*- lexical-binding: t -*-

(defconst akirak-git-commit-log-drawer-start-re
  (rx bol (* blank) ":GITCOMMITS:" (or blank eol)))

;;;###autoload
(define-minor-mode akirak-git-commit-log-to-org-clock-mode
  "Log commit messages to the clocked Org entry."
  :lighter " LogCommitsToOrg"
  :global t
  (cond
   ((bound-and-true-p akirak-git-commit-log-to-org-clock-mode)
    (add-hook 'git-commit-post-finish-hook
              #'akirak-git-commit-log-to-org-clock))
   (t
    (remove-hook 'git-commit-post-finish-hook
                 #'akirak-git-commit-log-to-org-clock))))

(defun akirak-git-commit-log-to-org-clock ()
  (when (org-clocking-p)
    (let* ((message (ring-ref log-edit-comment-ring 0))
           (rev (magit-rev-parse "HEAD"))
           (rev-link (when (and (require 'orgit nil t)
                                (orgit-rev-store-1 rev))
                       (org-link-make-string
                        (plist-get org-store-link-plist :link)
                        (truncate-string-to-width rev 7))))
           (case-fold-search t))
      (org-with-point-at org-clock-marker
        (if (re-search-forward akirak-git-commit-log-drawer-start-re
                               (org-entry-end-position)
                               t)
            (progn
              (re-search-forward org-drawer-regexp)
              (beginning-of-line))
          ;; It's better for the drawer to not precede a drawer for
          ;; backlinks (if any), so the commit log drawer should be the last
          ;; drawer.
          (org-end-of-meta-data t)
          (re-search-backward (rx anything "\n") nil t)
          (goto-char (match-end 0))
          (insert ":GITCOMMITS:\n:END:\n")
          (beginning-of-line 0))
        (insert (akirak-git-commit--build-log-line
                  :rev-link rev-link
                  :message message)
                "\n")))))

(cl-defun akirak-git-commit--build-log-line (&key time rev-link message)
  (declare (indent 0))
  (format-spec "%t %m (%r)"
               `((?t . ,(format-time-string (org-time-stamp-format t t) time))
                 (?m . ,(akirak-git-commit--first-line message))
                 (?r . ,rev-link))))

(defun akirak-git-commit--first-line (string)
  (car (split-string string "\n")))

(provide 'akirak-git-commit)
;;; akirak-git-commit.el ends here
