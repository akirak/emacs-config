;; Customize modus-operandi theme.
;; See [[info:modus-themes#Override colors]]
;; and [[info:modus-themes#Customization Options]]
(progn
  (setq modus-themes-operandi-color-overrides
        '((bg-main . "#e1d9c2")
          (fg-main . "#333333"))

        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-paren-match '(bold intence)
        modus-themes-org-blocks 'gray-background

        modus-themes-links '(neutral-underline)

        modus-themes-headings
        '((1 . (background variable-pitch 1.5))
          (2 . (rainbow overline 1.1))
          (3 . (semibold))
          (4 . (italic))))
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(let ((file "~/org/config.el"))
  (when (file-exists-p file)
    (load-file file)))

(when (and custom-file (file-exists-p custom-file))
  (load custom-file))

(run-with-timer 1 nil
                (defun akirak/org-dog-load-on-startup ()
                  ;; I don't want to hide the init time in the echo area.
                  (let ((inhibit-message t))
                    (require 'org-dog nil t)
                    (when (fboundp 'org-dog-reload-files)
                      (org-dog-reload-files t)))))
