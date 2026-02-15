;;; akirak-pi.el --- Integration with Pi coding agent -*- lexical-binding: t -*-

(require 'akirak-transient)

(defconst akirak-pi-slash-commands
  '("/login"
    "/logout"
    "/model"
    "/scoped-models"
    "/settings"
    "/resume"
    "/new"
    "/name"
    "/session"
    "/tree"
    "/fork"
    "/compact"
    "/copy"
    "/export"
    "/share"
    "/reload"
    "/hotkeys"
    "/changelog"
    "/quit"
    "/exit"))

(defcustom akirak-pi-executable "pi"
  ""
  :type 'file)

(defcustom akirak-pi-default-args nil
  ""
  :type '(repeat string))

(defvar akirak-pi-directory nil)

(defvar akirak-pi-provider nil)

(transient-define-infix akirak-pi-set-provider ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-pi-provider
  :prompt "Provider: "
  :description "Provider")

(defvar akirak-pi-model nil)

(transient-define-infix akirak-pi-set-model ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-pi-model
  :prompt "Model: "
  :description "Model")

(defvar akirak-pi-thinking-level nil)

(transient-define-infix akirak-pi-set-thinking-level ()
  :class 'akirak-transient-choice-variable
  :cycle t
  :variable 'akirak-pi-thinking-level
  :choices '("off" "minimal" "low" "medium" "high" "xhigh")
  :description "Thinking")

(defvar akirak-pi-models-scope nil)

(transient-define-infix akirak-pi-set-models-scope ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-pi-models-scope
  :prompt "Models (patterns): "
  :description "Model cycle")

(defvar akirak-pi-tools nil)

(transient-define-infix akirak-pi-set-tools ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-pi-tools
  :prompt "Tools: "
  :description "Tools")

(defvar akirak-pi-no-tools nil)

(transient-define-infix akirak-pi-toggle-no-tools ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-pi-no-tools
  :description "No tools")

;;;###autoload (autoload 'akirak-pi-transient "akirak-pi" nil 'interactive)
(transient-define-prefix akirak-pi-transient ()
  ["Options"
   ("-p" akirak-pi-set-provider)
   ("-m" akirak-pi-set-model)
   ("-t" akirak-pi-set-thinking-level)
   ("-s" akirak-pi-set-models-scope)
   ("-T" akirak-pi-set-tools)
   ("-n" akirak-pi-toggle-no-tools)
   ("-c" "Continue" "--continue")
   ("-r" "Resume" "--resume")
   ("-N" "No session" "--no-session")]
  ["Interactive sessions"
   ("p" "Open interactive shell" akirak-pi--open-shell)]
  (interactive)
  (setq akirak-pi-directory (akirak-shell-project-directory))
  (transient-setup 'akirak-pi-transient))

(cl-defun akirak-pi--open-shell (&key args)
  (interactive)
  (let ((root akirak-pi-directory))
    (akirak-shell-eat-new :dir root
                          :command (cons akirak-pi-executable
                                         (append akirak-pi-default-args
                                                 (when akirak-pi-provider
                                                   (list "--provider" akirak-pi-provider))
                                                 (when akirak-pi-model
                                                   (list "--model" akirak-pi-model))
                                                 (when akirak-pi-thinking-level
                                                   (list "--thinking" akirak-pi-thinking-level))
                                                 (when akirak-pi-models-scope
                                                   (list "--models" akirak-pi-models-scope))
                                                 (when akirak-pi-tools
                                                   (list "--tools" akirak-pi-tools))
                                                 (when akirak-pi-no-tools
                                                   (list "--no-tools"))
                                                 (transient-args 'akirak-pi-transient)
                                                 args))
                          :name (concat "pi-"
                                        (file-name-nondirectory
                                         (directory-file-name root))))))

(defun akirak-pi-complete-slash-command ()
  (completing-read "Pi command: " akirak-pi-slash-commands))

(provide 'akirak-pi)
;;; akirak-pi.el ends here
