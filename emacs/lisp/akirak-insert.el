;;; akirak-insert.el --- Skeletons -*- lexical-binding: t -*-

(require 'skeleton)

;;;###autoload (autoload 'akirak-insert-basename "akirak-insert")
(define-skeleton akirak-insert-basename
  "Insert the base name of the buffer." nil
  (file-name-base (buffer-file-name)))

;;;###autoload (autoload 'akirak-insert-basename-pascalcased "akirak-insert")
(define-skeleton akirak-insert-basename-pascalcased
  "Insert the base name of the buffer, pascal-cased." nil
  (progn
    (require 'string-inflection)
    (thread-last
      (file-name-base (buffer-file-name))
      (string-inflection-upper-camelcase-function))))

;;;###autoload (autoload 'akirak-insert-project-name "akirak-insert")
(define-skeleton akirak-insert-project-name
  "Insert the base name of the buffer." nil
  (thread-last
    (project-current)
    (project-root)
    (string-remove-suffix "/")
    (file-name-nondirectory)))

;;;###autoload (autoload 'akirak-insert-project-name-pascalcased "akirak-insert")
(define-skeleton akirak-insert-project-name-pascalcased
  "Insert the base name of the buffer, pascal-cased." nil
  (progn
    (require 'string-inflection)
    (thread-last
      (project-current)
      (project-root)
      (string-remove-suffix "/")
      (file-name-nondirectory)
      (string-inflection-upper-camelcase-function))))

;;;###autoload (autoload 'akirak-insert-from-lisp "akirak-insert")
(define-skeleton akirak-insert-from-lisp
  "Insert the result of a lisp expression." nil
  (format "%s" (eval (minibuffer-with-setup-hook
                         #'emacs-lisp-mode
                       (read--expression "Eval: ")))))

;;;###autoload (autoload 'akirak-insert-iso8601-date "akirak-insert")
(define-skeleton akirak-insert-iso8601-date
  "Insert date in the ISO-8601 format." nil
  (format-time-string "%F"))

;;;###autoload (autoload 'akirak-insert-iso8601-read-date "akirak-insert")
(define-skeleton akirak-insert-iso8601-read-date
  "Insert date in the ISO-8601 format." nil
  (org-read-date))

;;;###autoload (autoload 'akirak-insert-user-full-name "akirak-insert")
(define-skeleton akirak-insert-user-full-name
  "Insert the full name of the user" nil
  user-full-name)

;;;###autoload (autoload 'akirak-insert-user-mail-address "akirak-insert")
(define-skeleton akirak-insert-user-mail-address
  "Insert date in the ISO-8601 format." nil
  user-mail-address)

;;;###autoload (autoload 'akirak-insert-org-clock-heading "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-org-clock-heading
  "Insert the heading of the currently clocked entry." nil
  (save-current-buffer
    (org-with-point-at org-clock-marker
      (org-get-heading t t t t))))

(provide 'akirak-insert)
;;; akirak-insert.el ends here
