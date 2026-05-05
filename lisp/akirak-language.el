;;; akirak-language.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)

;;;###autoload (autoload 'akirak-language-transient "akirak-language" nil 'interactive)
(transient-define-prefix akirak-language-transient ()
  [:description
   (lambda ()
     (format "Switch the input method (current: %s)" current-input-method))
   :class transient-row
   ("j" "Japanese (custom)"
    (lambda ()
      (interactive)
      (set-input-method 'japanese-riben)))
   ("c" "Chinese (rime)"
    (lambda ()
      (interactive)
      (set-input-method 'rime)))]
  (interactive)
  (transient-setup 'akirak-language-transient))

(provide 'akirak-language)
;;; akirak-language.el ends here
