;;; akirak-copilot-cli.el --- Support for GitHub Copilot CLI -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)

;;;; Transient

(defvar akirak-copilot-cli-directory nil)

;;;###autoload (autoload 'akirak-copilot-cli-transient "akirak-copilot-cli" nil 'interactive)
(transient-define-prefix akirak-copilot-cli-transient ()
  "Start a terminal session for Copilot CLI"
  ["Options"
   ;; TODO: --allow-tool
   ;; TODO: --add-dir
   ("-r" "Resume" "--resume")
   ("-m" "Model" "--model=" :choices ("gpt-5"
                                      "claude-sonnet-4"
                                      "claude-sonnet-4.5"))]
  ["Actions"
   :class transient-row
   ("k" "Start a new session" akirak-copilot-cli--open-shell)]
  (interactive)
  (setq akirak-copilot-cli-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-copilot-cli-transient))

(defun akirak-copilot-cli--open-shell ()
  (interactive)
  (let ((root akirak-copilot-cli-directory)
        (args (transient-args 'akirak-copilot-cli-transient)))
    (akirak-shell-eat-new :dir root
                          :command (cons "copilot" args)
                          :name (concat "copilot-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(provide 'akirak-copilot-cli)
;;; akirak-copilot-cli.el ends here
