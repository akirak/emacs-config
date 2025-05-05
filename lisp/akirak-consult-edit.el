;;; akirak-consult-edit.el --- Consult command for editing source code -*- lexical-binding: t -*-

(require 'consult)

(defcustom akirak-consult-edit-fallback-function
  (lambda (prompt)
    (user-error "Not implemented. Input is %s" prompt))
  ""
  :type 'function)

(defvar akirak-consult-edit-org-tempo-sources
  `(:name "Template functions"
          ;; To suppress the default annotation, choose a different category
          ;; other than @='function.
          :category template-as-function
          :annotate ,(lambda (candidate)
                       (let ((doc (documentation (intern candidate))))
                         (when doc
                           (concat " " (car (split-string doc "\n"))))))
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

(provide 'akirak-consult-edit)
;;; akirak-consult-edit.el ends here
