;;; akirak-opencode.el --- Opencode support -*- lexical-binding: t -*-

(require 'akirak-shell)
(require 'transient)
(require 'akirak-transient)
(require 'akirak-ai-model)

;;;; Transient

(defvar akirak-opencode-directory nil)

(defvar akirak-opencode-model nil)

(transient-define-infix akirak-opencode-set-model ()
  :class 'akirak-transient-choice-variable
  :variable 'akirak-opencode-model
  :choices 'akirak-ai-model-list
  :description "Model")

;;;###autoload (autoload 'akirak-opencode-shell "akirak-opencode" nil 'interactive)
(transient-define-prefix akirak-opencode-shell ()
  "Start a terminal session for Opencode Code."
  ["Options"
   ("-m" akirak-opencode-set-model)]
  ["Actions"
   :class transient-row
   ("s" "Dispatch" akirak-opencode--open-shell)]
  (interactive)
  (setq akirak-opencode-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-opencode-shell))

(defun akirak-opencode--open-shell ()
  (interactive)
  (let ((root akirak-opencode-directory)
        (args (when akirak-opencode-model
                (list "--model" akirak-opencode-model))))
    (akirak-shell-eat-new :dir root
                          :command (cons "opencode" args)
                          :name (concat "opencode-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(provide 'akirak-opencode)
;;; akirak-opencode.el ends here
