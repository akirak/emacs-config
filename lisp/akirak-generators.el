;;; akirak-generators.el ---  -*- lexical-binding: t -*-

(require 'transient)

;;;###autoload (autoload 'akirak-generators "akirak-generators" nil 'interactive)
(transient-define-prefix akirak-generators ()
  [("b" "Random bytes" akirak-random-bytes)
   ("w" "Password" akirak-password-generate)
   ("p" "Passphrase" akirak-passphrase-generate)]
  (interactive)
  (transient-setup 'akirak-generators))

(provide 'akirak-generators)
;;; akirak-generators.el ends here
