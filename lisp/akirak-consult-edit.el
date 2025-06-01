;;; akirak-consult-edit.el --- Consult command for editing source code -*- lexical-binding: t -*-

(require 'consult)

(defcustom akirak-consult-edit-fallback-function
  #'akirak-consult-edit-prompt-prefix
  ""
  :type 'function)

(defvar akirak-consult-edit-org-tempo-source
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

(defvar akirak-consult-edit-suggestions nil)

(defvar akirak-consult-edit-suggestions-source
  `(:name "Suggestions"
          :category literal
          :annotate ,(lambda (candidate)
                       (concat " " (cdr (assoc candidate akirak-consult-edit-suggestions))))
          :items akirak-consult-generate-suggestions))

(defun akirak-consult-generate-suggestions ()
  (let ((suggestions
         (append (when (and (bound-and-true-p git-commit-mode)
                            (org-clocking-p))
                   (list (cons (replace-regexp-in-string
                                (rx bol (+ (any "-/." alnum)) ":" (+ blank))
                                ""
                                (org-link-display-format
                                 (org-entry-get (or org-clock-hd-marker
                                                    org-clock-marker)
                                                "ITEM")))
                               "Headline of the currently clocked Org entry.")))
                 (when-let* ((filename (if (minibufferp)
                                           (with-minibuffer-selected-window
                                             (akirak-consult-edit--filename))
                                         (akirak-consult-edit--filename))))
                   `((,filename
                      . "File name of the buffer.")
                     (,(file-name-base filename)
                      . "Base name of the buffer.")
                     (,(progn
                         (require 'string-inflection)
                         (thread-last
                           (file-name-base filename)
                           (string-inflection-upper-camelcase-function)))
                      . "Base name of the buffer, pascal-cased.")
                     (,(file-relative-name filename (vc-git-root default-directory))
                      . "Relative path of the file from the root.")))
                 (remq nil
                       (list (when-let* ((func (which-function)))
                               (cons func "Name of the function."))
                             (cons (format-time-string "%F")
                                   "Current date.")
                             (cons user-full-name "Name of the user.")
                             (when-let* ((pr (project-current)))
                               (cons (project-name pr)
                                     "Name of the project.")))))))
    (setq akirak-consult-edit-suggestions suggestions)
    (mapcar #'car suggestions)))

(defun akirak-consult-edit--filename ()
  (cond
   ((eq major-mode 'nov-mode)
    nov-file-name)
   ((derived-mode-p 'dired-mode)
    (dired-filename-at-point))
   (t
    (buffer-file-name (buffer-base-buffer)))))

(defvar akirak-consult-edit-sources
  '(akirak-consult-edit-org-tempo-source
    akirak-consult-edit-suggestions-source))

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
               (funcall func))))
          (literal
           (insert (car ent))))
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
