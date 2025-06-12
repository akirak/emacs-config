;;; akirak-claude.el --- Claude support -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)

;;;; Transient

(defvar akirak-claude-directory nil)

;;;###autoload (autoload 'akirak-claude-shell "akirak-claude" nil 'interactive)
(transient-define-prefix akirak-claude-code-shell ()
  "Start a terminal session for Claude Code."
  ["Options"
   ("-d" "Skip permissions" "--dangerously-skip-permissions")
   ("-c" "Continue" "--continue")
   ("-r" "Resume" "--resume")
   ("-m" "Model" "--model=" :choices ("sonnet" "opus"))]
  ["Actions"
   :class transient-row
   ("c" "Dispatch" akirak-claude--open-shell)]
  (interactive)
  (setq akirak-claude-directory (abbreviate-file-name (project-root (project-current))))
  (transient-setup 'akirak-claude-code-shell))

(defun akirak-claude--open-shell ()
  (interactive)
  (let ((root akirak-claude-directory)
        (args (transient-args 'akirak-claude-code-shell)))
    (akirak-shell-eat-new :dir root
                          :command (cons "claude" args)
                          :name (concat "claude-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(provide 'akirak-claude)
;;; akirak-claude.el ends here
