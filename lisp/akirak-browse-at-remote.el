;;; akirak-browse-at-remote.el ---  -*- lexical-binding: t -*-

(require 'akirak-transient)
(require 'browse-at-remote)

(defvar akirak-browse-at-remote-add-line-number t)

(transient-define-infix akirak-browse-at-remote-add-line-number ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-browse-at-remote-add-line-number
  :description "Add line number")

(defvar akirak-browse-at-remote-prefer-symbolic nil)

(transient-define-infix akirak-browse-at-remote-prefer-symbolic ()
  :class 'akirak-transient-flag-variable
  :variable 'akirak-browse-at-remote-prefer-symbolic
  :description "Prefer symbolic ref")

;;;###autoload (autoload 'akirak-browse-at-remote "akirak-browse-at-remote" nil 'interactive)
(transient-define-prefix akirak-browse-at-remote ()
  ["Options"
   :class transient-row
   ("-l" akirak-browse-at-remote-add-line-number)
   ("-s" akirak-browse-at-remote-prefer-symbolic)]
  [:class
   transient-row
   ("g" "Browse" akirak-browse-at-remote--browse)
   ("w" "Kill" akirak-browse-at-remote--kill)]
  (interactive)
  (unless (vc-git-root default-directory)
    (user-error "Not in a Git repository"))
  (transient-setup 'akirak-browse-at-remote))

(defun akirak-browse-at-remote--get-url ()
  (let ((browse-at-remote-add-line-number-if-no-region-selected
         akirak-browse-at-remote-add-line-number)
        (browse-at-remote-prefer-symbolic akirak-browse-at-remote-prefer-symbolic))
    (string-replace "/./" "/" (browse-at-remote-get-url))))

(defun akirak-browse-at-remote--kill ()
  (interactive)
  (kill-new (akirak-browse-at-remote--get-url))
  (message "Copied the permalink %s the line number"
           (if akirak-browse-at-remote-add-line-number
               "with"
             "without")))

(defun akirak-browse-at-remote--browse ()
  (interactive)
  (browse-url (akirak-browse-at-remote--get-url)))

(provide 'akirak-browse-at-remote)
;;; akirak-browse-at-remote.el ends here
