;;; akirak-password.el --- Password generator -*- lexical-binding: t -*-

(defconst akirak-password-default-symbol-characters
  "!@#$%^&*")

(defvar akirak-password-length 25)

(transient-define-infix akirak-password-set-password-length ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-password-length
  :description "Length in bytes")

(defvar akirak-password-avoid-big-i-and-o t)

(transient-define-infix akirak-password-avoid-big-i-and-o ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-password-avoid-big-i-and-o
  :description "Exclude I and O")

(defvar akirak-password-symbol-characters akirak-password-default-symbol-characters)

(transient-define-infix akirak-transient-set-symbols ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-password-symbol-characters
  :default akirak-password-default-symbol-characters
  :nullify t
  :prompt "Set symbol characters: "
  :description "Symbols")

;;;###autoload (autoload 'akirak-password-generate "akirak-password" nil 'interactive)
(transient-define-prefix akirak-password-generate ()
  "Generate a random password."
  ["Options"
   ("-l" akirak-password-set-password-length)]
  ["Password characters"
   :class transient-row
   ("-s" akirak-transient-set-symbols)
   ("-O" akirak-password-avoid-big-i-and-o)]
  ["Generate"
   :class transient-row
   ("w" "Copy a password" akirak-password--copy)
   ("i" "Insert a password" akirak-password--insert-password)]
  (interactive)
  (transient-setup 'akirak-password-generate))

(defun akirak-password--insert-password ()
  (interactive)
  (let ((string (akirak-password--generate)))
    (pcase (derived-mode-p 'eat-mode)
      (`eat-mode
       (eat-term-send-string-as-yank eat-terminal string))
      (_
       (insert string)))))

(defun akirak-password--copy ()
  (interactive)
  (require 'akirak-passage)
  (akirak-passage--copy-string (akirak-password--generate)))

(defun akirak-password--generate ()
  (shell-command-to-string
   (format-spec "tr -dc %c < /dev/urandom | head -c %l"
                `((?c . ,(shell-quote-argument
                          (concat (if akirak-password-avoid-big-i-and-o
                                      "a-zA-HJ-NP-Z"
                                    "a-zA-Z")
                                  "0-9"
                                  (or akirak-password-symbol-characters ""))))
                  (?l . ,akirak-password-length)))))

(provide 'akirak-random)
;;; akirak-random.el ends here
