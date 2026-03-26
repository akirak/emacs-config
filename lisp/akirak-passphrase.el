;;; akirak-passphrase.el --- Passphrase generator -*- lexical-binding: t -*-

(defvar akirak-passphrase-num-words 6)

(transient-define-infix akirak-passphrase--number-of-words ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-passphrase-num-words
  :description "Number of words")

(defvar akirak-passphrase-delimiter "")

(transient-define-infix akirak-passphrase--delimiter ()
  :class 'akirak-transient-string-variable
  :variable 'akirak-passphrase-delimiter
  :default ""
  :nullify nil
  :prompt "Delimiter: "
  :description "Delimiter")

(defvar akirak-passphrase-caps t)

(transient-define-infix akirak-passphrase--caps ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-passphrase-caps
  :description "Capitalize words")

(defvar akirak-passphrase-specials 0)

(transient-define-infix akirak-passphrase--special ()
  :class 'akirak-transient-number-variable
  :variable 'akirak-passphrase-specials
  :zero-is-nil t
  :description "Special chars")

;;;###autoload (autoload 'akirak-passphrase-generate "akirak-passphrase" nil 'interactive)
(transient-define-prefix akirak-passphrase-generate ()
  "Generate a random passphrase using diceware."
  ["Options"
   :class transient-row
   ("-n" akirak-passphrase--number-of-words)
   ("-d" akirak-passphrase--delimiter)
   ("-c" akirak-passphrase--caps)
   ("-s" akirak-passphrase--special)]
  ["Generate"
   :class transient-row
   ("w" "Copy a passphrase" akirak-passphrase--copy)
   ("i" "Insert a passphrase" akirak-passphrase--insert)]
  (interactive)
  (transient-setup 'akirak-passphrase-generate))

(defun akirak-passphrase--diceware-executable ()
  (or (executable-find "diceware")
      (when (executable-find "nix")
        (with-temp-buffer
          (call-process "nix-shell" nil (list t nil) nil
                        "-p" "diceware"
                        "--run" "command -v diceware")
          (string-trim (buffer-string))))
      (user-error "diceware is not available")))

(defun akirak-passphrase--generate ()
  (let ((args (list "-n" (number-to-string akirak-passphrase-num-words)
                    "-d" akirak-passphrase-delimiter)))
    (unless akirak-passphrase-caps
      (setq args (append args (list "--no-caps"))))
    (when (and akirak-passphrase-specials
               (> akirak-passphrase-specials 0))
      (setq args (append args (list "-s" (number-to-string akirak-passphrase-specials)))))
    (with-temp-buffer
      (apply #'call-process (akirak-passphrase--diceware-executable)
             nil (list t nil) nil args)
      (string-trim (buffer-string)))))

(defun akirak-passphrase--insert ()
  (interactive)
  (let ((string (akirak-passphrase--generate)))
    (pcase (derived-mode-p 'eat-mode)
      (`eat-mode
       (eat-term-send-string-as-yank eat-terminal string))
      (_
       (insert string)))))

(defun akirak-passphrase--copy ()
  (interactive)
  (require 'akirak-passage)
  (akirak-passage--copy-string (akirak-passphrase--generate)))

(provide 'akirak-passphrase)
;;; akirak-passphrase.el ends here
