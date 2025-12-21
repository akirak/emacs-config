;;; akirak-random.el --- Password generator through oepnssl -*- lexical-binding: t -*-

(defconst akirak-random-default-symbol-characters
  "!@#$%^&*")

(defvar akirak-random-password-length 25)

(transient-define-infix akirak-random-set-password-length ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-random-password-length
  :description "Length")

(defvar akirak-random-avoid-big-i-and-o t)

(transient-define-infix akirak-random-avoid-big-i-and-o ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-random-avoid-big-i-and-o
  :description "Exclude I and O")

(defvar akirak-random-symbol-characters akirak-random-default-symbol-characters)

(transient-define-infix akirak-transient-set-symbols ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-random-symbol-characters
  :default akirak-random-default-symbol-characters
  :nullify t
  :prompt "Set symbol characters: "
  :description "Symbols")

;;;###autoload (autoload 'akirak-random-password "akirak-random" nil 'interactive)
(transient-define-prefix akirak-random-password ()
  "Generate a random password."
  ["Options"
   ("-l" akirak-random-set-password-length)]
  ["Password characters"
   :class transient-row
   ("-s" akirak-transient-set-symbols)
   ("-O" akirak-random-avoid-big-i-and-o)]
  ["Generate"
   :class transient-row
   ("w" "Copy a password" akirak-random--copy-password)
   ("i" "Insert a password" akirak-random--insert-password)]
  (interactive)
  (transient-setup 'akirak-random-password))

(defun akirak-random--insert-password ()
  (interactive)
  (let ((string (akirak-random--generate-password)))
    (pcase (derived-mode-p 'eat-mode)
      (`eat-mode
       (eat-term-send-string-as-yank eat-terminal string))
      (_
       (insert string)))))

(defun akirak-random--copy-password ()
  (interactive)
  (require 'akirak-passage)
  (akirak-passage--copy-string (akirak-random--generate-password)))

(defun akirak-random--generate-password ()
  (shell-command-to-string
   (format-spec "tr -dc %c < /dev/urandom | head -c %l"
                `((?c . ,(shell-quote-argument
                          (concat (if akirak-random-avoid-big-i-and-o
                                      "a-zA-HJ-NP-Z"
                                    "a-zA-Z")
                                  "0-9"
                                  (or akirak-random-symbol-characters ""))))
                  (?l . ,akirak-random-password-length)))))

(provide 'akirak-random)
;;; akirak-random.el ends here
