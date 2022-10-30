;;; akirak-org-dwim.el --- Transient command for Org -*- lexical-binding: t -*-

(require 'transient)
(require 'octopus)
(require 'akirak-transient)

(declare-function truncate-string-to-width "mule-util")

(defmacro akirak-org-dwim--finalize-capture (&rest progn)
  `(let ((capture-buffer (akirak-org-clock--capture-buffer org-clock-marker)))
     ,@progn
     (when capture-buffer
       (with-current-buffer capture-buffer
         (org-capture-finalize)))))

;;;; Prefix

;;;###autoload (autoload 'akirak-org-dwim-on-clock "akirak-org-dwim" nil 'interactive)
(transient-define-prefix akirak-org-dwim-on-clock ()
  [:description
   akirak-org-dwim--clock-description
   :class transient-row
   :if org-clocking-p
   ("d" "Mark as done" akirak-org-dwim-clock-done)
   ("r" "Mark as REVIEW" akirak-org-dwim-clock-set-review)
   ("O" "Clock out" akirak-org-dwim-clock-out)
   ("o" "Display clocked entry" akirak-org-clock-open)
   ;; Save the current window configuration
   ]
  [:description
   akirak-org-dwim--memento-block-description
   :if akirak-org-dwim--memento-current-block-p
   :class transient-row
   ("C-o" "Finish" org-memento-finish-block)
   ("C-c" "Stop" org-memento-stop-block)]
  [:description
   akirak-org-dwim--memento-description
   :if akirak-org-dwim--memento-p
   :class transient-row
   ("t" "Open the journal" org-memento-open-journal)
   ("T" "Timeline" org-memento-timeline)
   ("<tab>" "Start a block" org-memento-start-block
    :if-not akirak-org-dwim--memento-current-block-p)
   ("S" "Update status" org-memento-status)
   ("C" "Add event" org-memento-add-event)
   ("E" "Check out from the day" org-memento-checkout-from-day
    :if-not akirak-org-dwim--memento-current-block-p)]
  (interactive)
  (transient-setup 'akirak-org-dwim-on-clock))

(cl-defmethod octopus--dispatch ((_cmd (eql 'akirak-org-dwim-on-clock))
                                 file)
  (setq akirak-org-dwim-selected-files file)
  (transient-setup 'akirak-org-dwim-on-clock))

;;;; Descriptions and predicates

(defun akirak-org-dwim--clock-description ()
  (org-with-clock-position (list org-clock-marker)
    (let ((olp (org-get-outline-path t)))
      (format "Clock: \"%s\" in %s (%s)"
              (car (last olp))
              (buffer-name)
              (substring-no-properties
               (org-format-outline-path (butlast olp)))))))

(defun akirak-org-dwim--files-p ()
  (and akirak-org-dwim-selected-files
       (not (org-clocking-p))))

(defun akirak-org-dwim--files-description ()
  (let ((files (thread-last
                 (akirak-org-dwim--selected-files)
                 (mapcar #'file-name-base))))
    (truncate-string-to-width
     (concat "Selected files: " (string-join files " "))
     (frame-width)
     nil nil (truncate-string-ellipsis))))

(defun akirak-org-dwim--memento-p ()
  (and (require 'org-memento nil t)
       (or org-memento-status-data
           (progn
             (org-memento-status)
             t))))

(defun akirak-org-dwim--memento-description ()
  (condition-case-unless-debug _
      (concat "Memento: "
              (if-let (day (org-memento-today-as-block))
                  (let* ((started (org-memento-started-time day))
                         (ending (org-memento-ending-time day)))
                    (format-spec "%d %s%e%r"
                                 `((?d . ,(format-time-string "%F (%a)"))
                                   (?s . ,(format-time-string "%R" started))
                                   (?e . ,(if ending
                                              (format-time-string "-%R" ending)
                                            ""))
                                   (?r . ,(if ending
                                              (format " (remaining %s)"
                                                      (org-duration-from-minutes
                                                       (/ (- ending (float-time))
                                                          60)))
                                            "")))))
                "(not checked in)"))
    (error "(error: akirak-org-dwim--memento-description)")))

(defun akirak-org-dwim--memento-current-block-p ()
  (bound-and-true-p org-memento-current-block))

(defun akirak-org-dwim--memento-block-description ()
  (condition-case-unless-debug _
      (let ((block (org-memento-with-current-block
                     (org-memento-block-entry))))
        (format "Memento block: %s%s%s%s"
                (if-let (todo (org-element-property :TODO
                                                    (org-memento-headline-element block)))
                    (concat todo " ")
                  "")
                org-memento-current-block
                (if-let (category (org-element-property :memento_category
                                                        (org-memento-headline-element block)))
                    (format " (%s)" category)
                  "")
                (let* ((started (org-memento-started-time block))
                       (ending (org-memento-ending-time block))
                       (remaining (when ending
                                    (/ (- ending (float-time))
                                       60))))
                  (format-spec " %s%e%r"
                               `((?s . ,(format-time-string "%R" started))
                                 (?e . ,(if ending
                                            (format-time-string "-%R" ending)
                                          ""))
                                 (?r . ,(cond
                                         ((null remaining)
                                          "")
                                         ((> remaining 0)
                                          (format " (remaining %s)"
                                                  (org-duration-from-minutes remaining)))
                                         ((<= remaining 0)
                                          (propertize (format " (%d minutes exceeding)"
                                                              (- remaining))
                                                      'face
                                                      (if (> remaining 0)
                                                          'font-lock-warning-face
                                                        'default))))))))))
    (error "(error: akirak-org-dwim--memento-block-description)")))

(defun akirak-org-dwim--memento-status-description ()
  (format "No current block"))

;;;; Suffix commands

(defun akirak-org-dwim-clock-out ()
  (interactive)
  (if-let (capture-buffer (akirak-org-clock--capture-buffer org-clock-marker))
      (with-current-buffer capture-buffer
        (org-capture-finalize))
    (org-clock-out)))

(defun akirak-org-dwim-clock-done ()
  (interactive)
  (org-with-clock-position (list org-clock-marker)
    (akirak-org-dwim--finalize-capture
     (org-todo 'done))))

(defun akirak-org-dwim-clock-set-review ()
  (interactive)
  (org-with-clock-position (list org-clock-marker)
    (akirak-org-dwim--finalize-capture
     ;; If you add the todo keyword to `org-clock-out-when-done', `org-clock-out'
     ;; will be tirggered when you switch to the state.
     (org-todo "REVIEW"))))

(provide 'akirak-org-dwim)
;;; akirak-org-dwim.el ends here
