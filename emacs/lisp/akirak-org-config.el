;;; akirak-org-config.el --- Personal Org configuration -*- lexical-binding: t -*-

(defconst akirak-org-task-type-tags
  '("@journaling"
    "@exercises"
    "@scripting"
    "@troubleshooting"
    "@coding"
    "@tedius"
    "@operation"
    "@speaking"
    "@reading"
    "@writing"
    "@tinkering"
    "@shopping"
    "@assessment"
    "@study"
    "@prototyping"
    "@housekeeping"
    "@maintenance"
    "@tutorial"
    "@practising"
    "@planning"
    "@topic"
    "@organising"
    "@research"
    "@configuration"
    "@news"
    "@idea"
    "@administration"
    "@contribution"
    "@project"
    "@distraction"))

;;;###autoload
(defun akirak-org-config-setup ()
  (akirak-org-setup-todo)
  (akirak-org-setup-tags)
  (setq octopus-view-function #'akirak-org-ql-search-files)
  (eval-after-load 'oahu
    #'akirak-org-config-setup-oahu))

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

(defun akirak-org-setup-tags ()
  (setq org-tag-persistent-alist
        `((:startgrouptag)
          ("@task")
          ("@epic")
          (:grouptags)
          ,@(mapcar #'ensure-list akirak-org-task-type-tags)
          (:endgrouptag)
          (:startgrouptag)
          ("@note")
          (:grouptags)
          (:endgrouptag)))

  (setq org-agenda-hide-tags-regexp
        (rx-to-string `(or ,@akirak-org-task-type-tags))))

(defun akirak-org-dog-project-context (project)
  (require 'project)
  (pcase (thread-last
           project
           project-root
           abbreviate-file-name
           file-name-split)
    ((or `("~" "work2" "learning" ,group ,name "")
         `("~" "work2" "learning" ,group ""))
     (make-org-dog-context-in-directory
      :directory (list "projects/" "technology/" "programming/")
      :filenames (append (list group)
                         (when name
                           (list name)))))
    ((or `("~" "work2" ,_ ,group ,name "")
         `("~" "work2" ,_ ,group ""))
     (make-org-dog-context-in-directory
      :directory (list "projects/" "programming/")
      :filenames (append (when name
                           (list (concat name "-dev")
                                 name))
                         (list (concat group "-dev")
                               group))))
    (`("~" ,name "")
     (make-org-dog-context-in-directory
      :directory (list "projects/" "programming/" "skills/")
      :filenames (list name)))))

(defun akirak-org-config-setup-oahu ()
  (let* ((query1 `(and (not (tags "ARCHIVE"))
                       (or (and (level 2)
                                (parent (heading "Backlog")))
                           (and (ancestors (heading "Activities" "Index"))
                                (todo))
                           (and (tags "@bestof")
                                (parent (heading-regexp ,(rx (repeat 4 digit)))))
                           (clocked :from -14))))
         (simple-backlog '(and (level 2)
                               (parent (heading "Backlog"))
                               (not (tags "ARCHIVE"))))
         (backlog `("Backlog" akirak-oahu-org-ql-search
                    ,(copy-tree simple-backlog)
                    :title "Backlog"
                    :super-groups
                    ((:tag "@epic")
                     (:todo "DONE")
                     (:todo "REVIEW")
                     (:auto-planning t)
                     (:todo "UNDERWAY")
                     (:todo "NEXT")
                     (:todo "IDEATE")
                     (:todo ("RECUR" "CASUAL"))
                     (:todo ("WAITING" "STOPPED"))
                     (:priority "C" :order 80)
                     (:priority "A")
                     (:auto-tags t)
                     (:todo t)
                     (:not (:todo t)))))
         (learning `("Learning" akirak-oahu-org-ql-search
                     ,(copy-tree query1)
                     :super-groups
                     ((:tag "@epic")
                      (:name "Recently archived" :datetree t)
                      (:todo "REVIEW")
                      (:todo "UNDERWAY")
                      (:tag "@study")
                      (:tag "@exercises")
                      (:tag "@basics")
                      (:tag "@bestof")
                      (:tag "@tutorial")
                      (:tag "@news")
                      (:tag "@documentation")
                      (:tag "@research")
                      (:priority "A")
                      (:auto-tags t)
                      (:anything t))))
         (reading `("@reading" akirak-oahu-org-ql-search
                    (and (tags "@reading")
                         ,(copy-tree query1))
                    :super-groups
                    ((:todo "REVIEW")
                     (:todo "UNDERWAY")
                     (:todo "NEXT")
                     (:auto-tags t))))
         (tutorial `("@tutorial" akirak-oahu-org-ql-search
                     (and (tags "@tutorial")
                          ,(copy-tree query1))
                     :super-groups
                     ((:todo "UNDERWAY")
                      (:todo "NEXT")
                      (:priority "A")
                      (:auto-tags t))))
         (exercises `("@exercises" akirak-oahu-org-ql-search
                      (and (tags "@exercises")
                           ,(copy-tree query1))
                      :super-groups
                      ((:tag "@epic")
                       (:todo "UNDERWAY")
                       (:todo "NEXT")
                       (:priority "A")
                       (:auto-tags t))))
         (documentation `("@documentation" akirak-oahu-org-ql-search
                          (and (tags "@documentation")
                               ,(copy-tree query1))
                          :super-groups
                          ((:todo "UNDERWAY")
                           (:todo "NEXT")
                           (:priority "A")
                           (:auto-tags t))))
         (backlog-clocked-history `("Backlog (by last clocked date)"
                                    akirak-oahu-org-ql-search
                                    (and (level 2)
                                         (parent (heading "Backlog"))
                                         (clocked))
                                    :super-groups
                                    ((:auto-clock t))))
         (backlog-history `("Backlog (by timestamp)"
                            akirak-oahu-org-ql-search
                            (and (level 2)
                                 (parent (heading "Backlog")))
                            :super-groups
                            ((:auto-ts reverse))))
         (meaningful-history `("Meaningful items (by timestamp)"
                               akirak-oahu-org-ql-search
                               (or (todo)
                                   (done)
                                   (and (level 2)
                                        (parent (heading "Backlog")))
                                   (datetree))
                               :super-groups
                               ((:auto-ts reverse))))
         (datetree `("Datetree" akirak-oahu-org-ql-search
                     (datetree)
                     :super-groups
                     ,(copy-tree '((:auto-reverse-parent t)))))
         (weektree `("Weektree" akirak-oahu-org-ql-search
                     (weektree)
                     :super-groups
                     ((:auto-reverse-parent t))))
         ;; reading list
         ;; watch list
         )
    (setq oahu-process-alist
          `((Project
             ;; A function that takes no argument and returns a non-nil value if the
             ;; process can be activated.
             :context project-current
             :context-selector
             (lambda ()
               (let ((default-directory (completing-read "Project: "
                                                         (project-known-project-roots)
                                                         nil t)))
                 (project-current)))
             ;; A function that takes the returned value of the context function and
             ;; returns a list of Org files.
             :files akirak-oahu-dog-project-files
             :views
             (,(copy-tree backlog)
              ,(copy-tree backlog-clocked-history)
              ,(copy-tree backlog-history)
              ,(copy-tree datetree)))
            (Org
             :context akirak-oahu-current-org-file
             :context-selector
             (lambda ()
               (let ((file (oref (org-dog-file-object (org-dog-complete-file)) relative))
                     (trees (yes-or-no-p "Include files linked from the header?")))
                 (if trees
                     (list :trees file)
                   (list file))))
             :files akirak-oahu-org-files
             :views
             (,(copy-tree learning)
              ,(copy-tree backlog)
              ,(copy-tree tutorial)
              ,(copy-tree exercises)
              ,(copy-tree reading)
              ,(copy-tree documentation)
              ,(copy-tree backlog-clocked-history)
              ,(copy-tree backlog-history)
              ,(copy-tree meaningful-history)
              ,(copy-tree datetree)
              ,(copy-tree weektree)))
            (OrgBookmark
             :context ignore
             :context-selector
             (lambda ()
               (org-placeholder-read-bookmark-name "Org bookmark: "))
             :files ignore
             :views
             (("View" akirak-oahu-org-placeholder-view)))
            (Review
             :context (lambda () "5:00")
             :context-selector (lambda ()
                                 (akirak-oahu-read-date "Start date:"))
             :files akirak-oahu-org-files-span
             :views
             (("Done in backlog (grouped by date)" akirak-oahu-ql-search-for-span
               (lambda (time-string)
                 `(and (done)
                       (level 2)
                       (parent (heading "Backlog"))
                       (closed :from ,time-string)))
               :super-groups
               ((:auto-ts t)))
              ("Marked for review (grouped by date)" akirak-oahu-ql-search-for-span
               (lambda (_time-string)
                 `(and (or (todo "REVIEW") (done))
                       (parent (heading "Backlog"))))
               :super-groups
               ((:auto-ts t)))
              ("Clocked (grouped by category)" akirak-oahu-ql-search-for-span
               (lambda (time-string)
                 `(clocked :from ,time-string))
               :super-groups
               ((:auto-category t)))
              ("New (grouped by category)" akirak-oahu-ql-search-for-span
               (lambda (time-string)
                 `(and (ts-inactive :from ,time-string)
                       (not (clocked :from ,time-string))))
               :super-groups
               ((:auto-category t)))))))

    (setq oahu-memento-view-derive-fn
          (cl-function
           (lambda (type argument views &key title tags properties)
             (or (car (seq-intersection views tags #'string-equal-ignore-case))
                 (pcase type
                   (`Project
                    "Backlog")
                   (`Org
                    (pcase (seq-find #'stringp argument)
                      ((rx bol "projects/")
                       "Backlog")
                      ((rx bol (or "programming/"
                                   "languages/"
                                   "technology/"))
                       "Learning")))
                   (`OrgBookmark
                    "View")))))))

  (with-eval-after-load 'org-ql-view
    (when (ignore-errors
            (transient-get-suffix 'org-ql-view-dispatch 'org-ql-view))
      (transient-replace-suffix 'org-ql-view-dispatch 'org-ql-view
        `("v" "Oahu view"
          ,(defun akirak-org-ql-view-oahu-view ()
             (interactive)
             (oahu-view 'Org org-ql-view-buffers-files)))))))

(defun akirak-oahu-org-files-span (span)
  (let ((start-string (thread-last
                        (akirak-oahu-start-time-for-span span)
                        (format-time-string "%F"))))
    (thread-last
      (org-dog-select 'absolute `(ts-since-date ,start-string))
      (delete org-memento-file))))

(defun akirak-oahu-start-time-for-span (span)
  (let ((decoded-time (parse-time-string (org-read-date nil nil
                                                        (format "%s" span)))))
    (setf (decoded-time-hour decoded-time) org-extend-today-until)
    (setf (decoded-time-minute decoded-time) 0)
    (setf (decoded-time-second decoded-time) 0)
    (encode-time decoded-time)))

(defun akirak-oahu-read-date (&optional prompt)
  (let* ((today (decode-time))
         (default (encode-time
                   (if (< (decoded-time-hour today) org-extend-today-until)
                       (decoded-time-add today (make-decoded-time :day -1))
                     today))))
    (org-read-date nil nil nil prompt default)))

(defun akirak-oahu-org-files (files)
  (let ((files1 (member :trees files)))
    (cl-flet
        ((resolve (file)
           (if (file-name-absolute-p file)
               file
             (or (org-dog-resolve-relative-file file)
                 (error "Failed to resolve file %s" file)))))
      (append (thread-last
                (seq-take files (- (length files)
                                   (length files1)))
                (mapcar #'resolve))
              (when files1
                (mapcar #'car (org-dog-overview-scan
                               (mapcar #'resolve (cdr files1))
                               :fast t)))))))

(defun akirak-oahu-current-org-file ()
  (and (eq major-mode 'org-mode)
       (when-let (obj (org-dog-buffer-object))
         (list (oref obj relative)))))

(defun akirak-oahu-dog-project-files (pr)
  (require 'org-dog-context)
  (or (when-let (ctx (funcall (plist-get (cdr (assq 'project org-dog-context-alist))
                                         :callback)
                              pr))
        (thread-last
          (org-dog-context-file-objects ctx)
          (mapcar (lambda (obj)
                    (oref obj absolute)))))
      ;; (user-error "No associated Org file for %s" pr)
      ))

(defun akirak-oahu-ql-search-for-span (span files make-query &rest options)
  (apply #'org-ql-search files
         (thread-last
           (akirak-oahu-start-time-for-span span)
           (format-time-string "%F %R")
           (funcall make-query))
         options))

(defun akirak-oahu-org-placeholder-view (args)
  (apply #'org-placeholder-view args))

(defun akirak-oahu-org-ql-search (_ buffers-files query &rest args)
  (let ((buffer "*oahu org-ql search*"))
    (apply #'org-ql-search buffers-files query
           :buffer buffer
           args)
    buffer))

(defun akirak-org-ql-search-files (files)
  (oahu-alternative-view 'Org (if (stringp files)
                                  (list files)
                                files)))

;;;; Programmable completion

(defun akirak-org-entry-annotation (org-marker)
  (when (member "@glossary" (org-get-tags org-marker))
    (org-with-point-at org-marker
      (org-end-of-meta-data t)
      (unless (looking-at org-heading-regexp)
        (let ((element (org-element-at-point (point))))
          (pcase (org-element-type element)
            (`paragraph
             (concat " "
                     (thread-first
                       (buffer-substring-no-properties
                        (org-element-property :begin element)
                        (org-element-property :end element))
                       (string-trim)
                       (propertize 'face 'italic))))))))))

(provide 'akirak-org-config)
;;; akirak-org-config.el ends here
