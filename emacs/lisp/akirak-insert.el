;;; akirak-insert.el --- Skeletons -*- lexical-binding: t -*-

(require 'skeleton)

;;;###autoload
(defun akirak-insert ()
  "Complete an insertion command."
  (interactive)
  (let ((command (read-extended-command-1 "Insert: " "akirak-insert- ")))
    (call-interactively (intern command))))

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

;;;###autoload (autoload 'akirak-insert-directory "akirak-insert")
(define-skeleton akirak-insert-directory
  "Insert the current default directory." nil
  (expand-file-name default-directory))

;;;###autoload (autoload 'akirak-insert-abbreviated-directory "akirak-insert")
(define-skeleton akirak-insert-abbreviated-directory
  "Insert the current default directory, abbreviated." nil
  (abbreviate-file-name default-directory))

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
  "Insert the full name of the user." nil
  user-full-name)

;;;###autoload (autoload 'akirak-insert-user-mail-address "akirak-insert")
(define-skeleton akirak-insert-user-mail-address
  "Insert the user's email address in the ISO-8601 format." nil
  user-mail-address)

;;;###autoload (autoload 'akirak-insert-org-clock-heading "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-org-clock-heading
  "Insert the heading of the currently clocked entry." nil
  (if (and (require 'org-clock nil t)
           (org-clocking-p))
      (save-current-buffer
        (require 'ol)
        (org-link-display-format (org-entry-get org-clock-marker "ITEM")))
    (user-error "Not clocking in")))

;;;###autoload (autoload 'akirak-insert-window-title "akirak-insert" nil 'interactive)
(define-skeleton akirak-insert-window-title
  "Insert a string from the title of a window on the desktop"
  (require 'akirak-window-system)
  (replace-regexp-in-string
   (rx " — Mozilla Firefox" eos)
   ""
   (akirak-window-system-complete-title "Window title: ")))

(provide 'akirak-insert)
;;; akirak-insert.el ends here
