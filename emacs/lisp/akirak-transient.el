;;; akirak-transient.el --- Transient definitions -*- lexical-binding: t -*-

;; Based on sample code in a comment by SataMaxx on Reddit at
;; <https://www.reddit.com/r/emacs/comments/mx6xs2/comment/gvoldon/>

;;;; akirak-transient-variable

(defclass akirak-transient-variable (transient-variable)
  ((variable :initarg :variable)))

(cl-defmethod transient-init-value ((obj akirak-transient-variable))
  (oset obj value (eval (oref obj variable))))

(cl-defmethod transient-infix-read ((obj akirak-transient-variable))
  (read-from-minibuffer "New value: " (oref obj value)))

(cl-defmethod transient-infix-set ((obj akirak-transient-variable) value)
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-value ((obj akirak-transient-variable))
  (let ((value (oref obj value)))
    (concat
     (propertize "(" 'face 'transient-inactive-value)
     (propertize value 'face 'transient-value)
     (propertize ")" 'face 'transient-inactive-value))))

;;;; akirak-transient-choice-variable

(defclass akirak-transient-choice-variable (akirak-transient-variable)
  ((choices :initarg :choices)))

(cl-defmethod transient-infix-read ((obj akirak-transient-choice-variable))
  (let ((choices (oref obj choices)))
    (if-let* ((value (oref obj value))
              (notlast (cadr (member value choices))))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-format-value ((obj akirak-transient-choice-variable))
  (let* ((variable (oref obj variable))
         (choices  (oref obj choices))
         (value    (oref obj value)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (choice)
                  (propertize choice 'face (if (equal choice value)
                                               (if (member choice choices)
                                                   'transient-value
                                                 'font-lock-warning-face)
                                             'transient-inactive-value)))
                (if (and value (not (member value choices)))
                    (cons value choices)
                  choices)
                (propertize "|" 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

;;;; akirak-transient-flag-variable

(defclass akirak-transient-flag-variable (akirak-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj akirak-transient-flag-variable))
  (not (oref obj value)))

(cl-defmethod transient-format ((obj akirak-transient-flag-variable))
  (format-spec " %k %d"
               `((?k . ,(transient-format-key obj))
                 (?d . ,(propertize (oref obj description)
                                    'face
                                    (if (oref obj value)
                                        'transient-value
                                      'transient-inactive-value))))))

(provide 'akirak-transient)
;;; akirak-transient.el ends here
