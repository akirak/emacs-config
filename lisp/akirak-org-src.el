;;; akirak-org-src.el --- Extra integration for Org source blocks -*- lexical-binding: t -*-

(require 'org)
(require 'org-src)
(require 'org-element)
(require 'akirak-transient)
(require 'akirak-subr)

(defvar akirak-org-src--current-block nil)

;;;###autoload (autoload 'akirak-org-src-transient "akirak-org-src" nil 'interactive)
(transient-define-prefix akirak-org-src-transient ()
  ["General"
   ("l" akirak-org-src-set-language)]
  (interactive)
  (akirak-org-src--ensure-current-block)
  (transient-setup 'akirak-org-src-transient))

(defun akirak-org-src--ensure-current-block ()
  (setq akirak-org-src--current-block (org-element-at-point-no-context))
  (unless (and akirak-org-src--current-block
               (eq 'src-block (org-element-type akirak-org-src--current-block)))
    (user-error "Not on a source block")))

(defun akirak-org-src--language ()
  (org-element-property :language akirak-org-src--current-block))

(transient-define-suffix akirak-org-src-set-language (language)
  :description (lambda ()
                 (format "Language (%s)" (akirak-org-src--language)))
  (interactive
   (list
    (let ((current (akirak-org-src--language)))
      (string-remove-suffix "-mode" (akirak-complete-major-mode "Language: " current nil
                                                         :org-src-langs t)))))
  (akirak-org-src--set-language language))

(defun akirak-org-src--set-language (language)
  (akirak-org-src--ensure-current-block)
  (let ((case-fold-search t))
    (if (org-match-line org-babel-src-block-regexp)
        (replace-match language nil nil nil 2)
      (error "Failed to match against org-babel-src-block-regexp"))))

(provide 'akirak-org-src)
;;; akirak-org-src.el ends here
