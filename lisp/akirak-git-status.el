;;; akirak-git-status.el ---  -*- lexical-binding: t -*-

(require 'magit-section)
(require 'taxy)
(require 'akirak-project)

(defun akirak-git-status--maybe-scan-projects ()
  (unless (and akirak-project-last-rescan
               (< (- (float-time)
                     (float-time akirak-project-last-rescan))
                  3600))
    (akirak-project-rescan)))

(defun akirak-git-status-dirty-projects ()
  "List dirty projects."
  (interactive)
  (project-known-project-roots)
  )

(provide 'akirak-git-status)
;;; akirak-git-status.el ends here
