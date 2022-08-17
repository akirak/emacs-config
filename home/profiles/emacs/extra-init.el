(require 'sanityinc-tomorrow-eighties-theme)
(load-theme 'sanityinc-tomorrow-eighties t)

;; (require 'poet-theme)
;; (load-theme 'poet t)

(let ((file "~/org/config.el"))
  (when (file-exists-p file)
    (load-file file)))

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))
