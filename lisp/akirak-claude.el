;;; akirak-claude.el --- Claude support -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)

;;;; Transient

(defvar akirak-claude-directory nil)

;;;###autoload (autoload 'akirak-claude-code-shell "akirak-claude" nil 'interactive)
(transient-define-prefix akirak-claude-code-shell ()
  "Start a terminal session for Claude Code."
  ["Options"
   ("-d" "Skip permissions" "--dangerously-skip-permissions")
   ("-c" "Continue" "--continue")
   ("-r" "Resume" "--resume")
   ("-m" "Model" "--model=" :choices ("claude-sonnet-4-20250514"
                                      "claude-opus-4-20250514"))]
  ["Actions"
   :class transient-row
   ("c" "Dispatch" akirak-claude--open-shell)]
  (interactive)
  (setq akirak-claude-directory (akirak-shell-project-directory))
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

;;;; Other commands

(defcustom akirak-claude-code-default-options
  ;; I don't have Claude Max at present
  '("--model=sonnet")
  "Default command line options for Claude Code."
  :type '(repeat string))

;;;###autoload
(defun akirak-claude-code-default ()
  (interactive)
  (let* ((root (akirak-shell-project-directory))
         (buffers (seq-filter (apply-partially #'akirak-shell-buffer-in-dir-p root)
                              (buffer-list))))
    (if buffers
        (let ((buffer (completing-read "Shell: " (mapcar #'buffer-name buffers)
                                       nil t)))
          (pop-to-buffer buffer))
      (akirak-shell-eat-new :dir root
                            :command (cons "claude" akirak-claude-code-default-options)
                            :name (concat "claude-"
                                          (file-name-nondirectory
                                           (directory-file-name root)))))))

(provide 'akirak-claude)
;;; akirak-claude.el ends here
