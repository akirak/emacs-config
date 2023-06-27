;;; akirak-org-config.el --- Personal Org configuration -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-org-setup-todo ()
  (setq-default org-todo-keywords
                ;; Normal workflow
                '((sequence
                   "TODO(t)"
                   "NEXT(n!)"
                   "UNDERWAY(u!)"
                   "REVIEW(r!)"
                   "|"
                   "DONE(d)"
                   "INVALID(v)")
                  ;; Alternative flow
                  (type
                   "STOPPED(p@)"
                   "WAITING(w@/!)"
                   "REWORK(K@/!)"
                   "RECUR(R)"
                   "|"
                   "ARCHIVED(a@/!)")
                  (sequence
                   "NEEDFIX(k)"
                   "|"
                   "LGTM(g)")
                  ;; Types
                  (type
                   "CASUAL(c)"
                   "EPIC(E)"
                   "IDEATE(i)")))

  (setq-default org-clock-out-when-done
                '("DONE"
                  "REVIEW"
                  "ARCHIVED"))

  (setq-default org-todo-state-tags-triggers
                '(("ARCHIVED" ("ARCHIVE" . t))
                  ("INVALID" ("ARCHIVE" . t))
                  ("TODO" ("noexport"))
                  ("DONE" ("noexport"))
                  ("EPIC" ("@epic" . t))
                  ("REWATCH" ("@rewatch" . t))
                  ("IDEATE" ("@idea" . t))
                  ("STARTED" ("@ideate"))
                  ("PURCHASE" ("@wishlist" . t))))

  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground "DodgerBlue"))
          ("WATCH" . (:foreground "DodgerBlue"))
          ("NEXT" . (:foreground "MediumPurple3"))
          ("MAINT" . (:foreground "green4"))
          ("UNDERWAY" . (:foreground "DarkOrange" :slant italic))
          ("WATCHING" . (:foreground "DarkOrange" :slant italic))
          ("REWATCH" . (:foreground "DodgerBlue" :underline t))
          ("REREAD" . (:foreground "DodgerBlue" :underline t))
          ("REVIEW" . (:foreground "ForestGreen" :underline t))
          ("EPIC" . (:foreground "gold3" :underline t))
          ("SKIM" . (:foreground "OrangeRed"))
          ;; Done-like states
          ("DONE" . (:foreground "black"))
          ("WATCHED" . (:foreground "black"))
          ("ARCHIVED" . (:foreground "gray55"))
          ;; Inactive states
          ("STOPPED" . (:foreground "DarkSlateGray4"))
          ("WAITING" . (:foreground "sienna4"))
          ;; Keywords specific to org-memento
          ("HALFWAY" . (:foreground "tan4" :weight bold))
          ("FAILED" . (:foreground "red2" :weight bold))
          ("EXTENDED" . (:foreground "DarkSlateGray4"))
          ("RETRY" . (:foreground "DarkSlateGray4"))))

  (defun akirak-org-clock-in-switch-state (kw)
    (when (and (not (and (boundp 'org-capture-mode) org-capture-mode))
               (not (equal (org-entry-get nil "STYLE") "habit")))
      (when (or (and (member kw (list "TODO"
                                      "NEXT"
                                      "STOPPED"))))
        "UNDERWAY")))

  (setq-default org-clock-in-switch-to-state #'akirak-org-clock-in-switch-state)

  (add-hook 'org-blocker-hook
            (defun akirak-org-block-casual (change-plist)
              (not (and (eq (plist-get change-plist :type)
                            'todo-state-change)
                        (equal (plist-get change-plist :from)
                               "CASUAL")
                        (member (plist-get change-plist :to)
                                org-done-keywords))))))

(provide 'akirak-org-config)
;;; akirak-org-config.el ends here
