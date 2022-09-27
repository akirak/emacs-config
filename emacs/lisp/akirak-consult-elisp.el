;;; akirak-consult-elisp.el ---  -*- lexical-binding: t -*-

(require 'consult)

(defvar akirak-consult-elisp-epkg-source
  `(:name "Epkg"
          :narrow ?e
          :category package
          ;; TODO: :state
          :items ,(lambda ()
                    (epkgs 'name))))

(defvar akirak-consult-elisp-config-source
  `(:name "Local packages"
          :narrow ?l
          :category package
          ;; TODO: :state
          :items ,(lambda ()
                    (require 'akirak-twist)
                    (thread-last
                      (akirak-twist-flake-nodes akirak-twist-lock-directory)
                      (mapcar #'car)
                      (mapcar #'symbol-name)))))

(defvar akirak-consult-elisp-package-sources
  '(akirak-consult-elisp-config-source
    akirak-consult-elisp-epkg-source))

;;;###autoload
(defun akirak-consult-elisp-package ()
  (interactive)
  (require 'epkg)
  (pcase (consult--multi akirak-consult-elisp-package-sources
                         :prompt "Elisp package: "
                         :sort nil)
    (`(,name . ,plist)
     (when (plist-get plist :match)
       (epkg-describe-package name)))))

(provide 'akirak-consult-elisp)
;;; akirak-consult-elisp.el ends here
