;;; akirak-codex.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)

(defcustom akirak-codex-executable "codex"
  ""
  :type 'file)

(defcustom akirak-codex-default-args
  '("--config" "preferred_auth_method=apikey")
  ""
  :type '(repeat string))

(defcustom akirak-codex-password-account
  "api.openai.com/apikey"
  ""
  :type 'string)

(defconst akirak-codex-slash-commands
  '())

(defvar akirak-codex-directory nil)

(defvar akirak-codex-reasoning-effort nil)

(transient-define-infix akirak-codex-set-reasoning-effort ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :variable 'akirak-codex-reasoning-effort
  :choices '("medium" "high")
  :description "Reasoning effort")

;;;###autoload (autoload 'akirak-codex-transient "akirak-codex" nil 'interactive)
(transient-define-prefix akirak-codex-transient ()
  ["Options"
   ("-m" "Model" "--model="
    :always-read t
    :init-value (lambda (obj) (oset obj value "gpt-5"))
    :choices ("gpt-5"
              "gpt-5-nano"
              "gpt-5-mini"))
   ("-r" akirak-codex-set-reasoning-effort)
   ("-s" "Sandbox" "--sandbox="
    :choices ("read-only"
              "workspace-write"
              "danger-full-access"))
   ("-a" "Ask for approval" "--ask-for-approval="
    :choices ("untrusted"
              "on-failure"
              "on-request"
              "never"))
   ("-f" "Full auto" "--full-auto")]
  ["Actions"
   ("x" "Open interactive shell" akirak-codex--open-shell)]
  (interactive)
  (setq akirak-codex-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-codex-transient))

(defun akirak-codex--open-shell ()
  (interactive)
  (let ((root akirak-codex-directory)
        (args (transient-args 'akirak-codex-transient)))
    (akirak-shell-eat-new :dir root
                          :command (cons akirak-codex-executable
                                         (append akirak-codex-default-args
                                                 (when akirak-codex-reasoning-effort
                                                   (list "--config"
                                                         (concat "model_reasoning_effort="
                                                                 akirak-codex-reasoning-effort)))
                                                 args))
                          :environment (akirak-codex-environment)
                          :name (concat "codex-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(defun akirak-codex-complete-slash-command ()
  (completing-read "Codex command: " akirak-codex-slash-commands))

(defun akirak-codex-environment ()
  (require 'akirak-passage)
  (akirak-passage-add-process-environment
   "OPENAI_API_KEY" akirak-codex-password-account))

(provide 'akirak-codex)
;;; akirak-codex.el ends here
