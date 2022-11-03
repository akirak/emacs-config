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
  (interactive)
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
