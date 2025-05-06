;;; akirak-consult-edit.el --- Consult command for editing source code -*- lexical-binding: t -*-

(require 'consult)

(defcustom akirak-consult-edit-fallback-function
  #'akirak-consult-edit-prompt-prefix
  ""
  :type 'function)

(defvar akirak-consult-edit-org-tempo-sources
  `(:name "Template functions"
          ;; To suppress the default annotation, choose a different category
          ;; other than @='function.
          :category template-as-function
          :annotate ,(lambda (candidate)
                       (when-let* ((func (intern-soft candidate))
                                   (doc (documentation func)))
                         (concat " " (car (split-string doc "\n")))))
          :items ,(lambda ()
                    (when (featurep 'akirak-org-tempo)
                      (akirak-org-tempo-ensure-loaded)
                      (mapcar (lambda (ent)
                                (symbol-name (cdr ent)))
                              (tempo-build-collection))))))

(defvar akirak-consult-edit-sources
  '(akirak-consult-edit-org-tempo-sources))

;;;###autoload
(defun akirak-consult-edit ()
  (interactive)
  (let ((ent (consult--multi akirak-consult-edit-sources
                             :prompt "Pick a template or enter a prompt: "
                             :sort nil)))
    (if (plist-get (cdr ent) :match)
        (cl-ecase (plist-get (cdr ent) :category)
          (template-as-function
           (let ((func (intern (car ent))))
             (if (commandp func)
                 (call-interactively func)
               (funcall func)))))
      (funcall akirak-consult-edit-fallback-function
               (car ent)))))

(defvar akirak-consult-edit-prompt nil)

(transient-define-prefix akirak-consult-edit-prompt-prefix (prompt)
  ["Gptel/elysium"
   (gptel--infix-provider)
   ("q" "Elysium" akirak-consult-edit--elysium)]
  (interactive "sPrompt: ")
  (setq akirak-consult-edit-prompt prompt)
  (transient-setup 'akirak-consult-edit-prompt-prefix))

(defun akirak-consult-edit--elysium ()
  (interactive)
  (elysium-query akirak-consult-edit-prompt))

(provide 'akirak-consult-edit)
;;; akirak-consult-edit.el ends here
