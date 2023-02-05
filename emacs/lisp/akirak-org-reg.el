;;; akirak-org-reg.el --- Transient for Org places -*- lexical-binding: t -*-

(require 'org)
(require 'org-capture)
(require 'transient)

(defcustom akirak-org-reg-action-fn #'akirak-embark-on-org-headline
  "Function called on an Org marker."
  :type 'function)

;;;; Infixes

(defclass akirak-org-reg-action (transient-variable)
  ((function-symbol :initarg :function-symbol)
   (format :initform " %k %d")))

(cl-defmethod transient-init-value ((obj akirak-org-reg-action)))

(cl-defmethod transient-infix-read ((obj akirak-org-reg-action))
  (not (eq akirak-org-reg-current-action (oref obj function-symbol))))

(cl-defmethod transient-infix-set ((obj akirak-org-reg-action) value)
  (setq akirak-org-reg-current-action (when value
                                      (oref obj function-symbol))))

(cl-defmethod transient-format ((obj akirak-org-reg-action))
  (let ((face (if (eq akirak-org-reg-current-action (oref obj function-symbol))
                  'transient-value
                'transient-inactive-value)))
    (format-spec (oref obj format)
                 `((?k . ,(transient-format-key obj))
                   (?d . ,(propertize (transient-format-description obj)
                                      'face face))))))

;; Provided as an example.

(transient-define-infix akirak-org-reg-goto-marker-infix ()
  :class 'akirak-org-reg-action
  :function-symbol 'org-goto-marker-or-bmk
  :description "Go to")

;;;; Suffixes

;;;;; Abstract class

(defclass akirak-org-reg-marker-suffix (transient-variable)
  ((format :initform " %k %d %p")))

(cl-defmethod transient-format ((obj akirak-org-reg-marker-suffix))
  (let ((marker (akirak-org-reg--get-marker obj)))
    (format-spec (oref obj format)
                 `((?k . ,(transient-format-key obj))
                   (?d . ,(transient-format-description obj))
                   (?p . ,(propertize (akirak-org-reg--format-marker marker)
                                      'face 'font-lock-doc-face))))))

(cl-defgeneric akirak-org-reg--get-marker (x))

(cl-defmethod transient-format ((obj akirak-org-reg-marker-suffix))
  (let ((marker (akirak-org-reg--get-marker obj)))
    (format-spec (oref obj format)
                 `((?k . ,(transient-format-key obj))
                   (?d . ,(transient-format-description obj))
                   (?p . ,(propertize (akirak-org-reg--format-marker marker)
                                      'face 'font-lock-doc-face))))))

(defun akirak-org-reg--format-marker (marker)
  (when (and (markerp marker)
             (buffer-live-p (marker-buffer marker)))
    (format "\"%s\" in %s"
            (org-with-point-at marker
              (and (looking-at org-complex-heading-regexp)
                   (org-link-display-format (match-string-no-properties 4))))
            (buffer-name (marker-buffer marker)))))

;;;;; Concrete marker class

(defclass akirak-org-reg-marker-variable (akirak-org-reg-marker-suffix)
  ((variable :initarg :variable)))

(cl-defmethod akirak-org-reg--get-marker ((obj akirak-org-reg-marker-variable))
  (symbol-value (oref obj variable)))

(cl-defmacro akirak-org-reg-define-marker-variable (suffix-name
                                                    &key variable description)
  (declare (indent 1))
  `(transient-define-suffix ,suffix-name ()
     :class 'akirak-org-reg-marker-variable
     :if (lambda ()
           (and (markerp (symbol-value ,variable))
                (buffer-live-p (marker-buffer (symbol-value ,variable)))))
     :description ,description
     :variable ,variable
     (interactive)
     (akirak-org-reg-dispatch ,variable)))

(akirak-org-reg-define-marker-variable akirak-org-reg-dispatch-on-clock
  :variable 'org-clock-hd-marker
  :description "Clock")

(akirak-org-reg-define-marker-variable akirak-org-reg-dispatch-on-last-capture
  :variable 'org-capture-last-stored-marker
  :description "Last capture")

;;;;; Class with a function returning a marker

(defclass akirak-org-reg-marker-function (akirak-org-reg-marker-suffix)
  ((function :initarg :function)))

(cl-defmethod akirak-org-reg--get-marker ((obj akirak-org-reg-marker-function))
  (funcall (oref obj function)))

(cl-defmacro akirak-org-reg-define-marker-function (suffix-name
                                                    &key function description)
  (declare (indent 1))
  `(transient-define-suffix ,suffix-name ()
     :class 'akirak-org-reg-marker-function
     :if (lambda () (markerp (funcall ,function)))
     :description ,description
     :function ,function
     (interactive)
     (akirak-org-reg-dispatch (funcall ,function))))

(akirak-org-reg-define-marker-function akirak-org-reg-dispatch-on-memento
  :function 'akirak-org-reg--memento-marker
  :description "Memento")

(defun akirak-org-reg--memento-marker ()
  (when (bound-and-true-p org-memento-current-block)
    (org-memento-marker (org-memento--current-block))))

;;;;; Registers

(defun akirak-org-reg-register-suffixes (_children)
  (let (entries)
    (pcase-dolist (`(,key . ,marker) register-alist)
      (when-let* ((filename (and (markerp marker)
                                 (buffer-live-p (marker-buffer marker))
                                 (buffer-file-name (org-base-buffer (marker-buffer marker)))))
                  (description (and (string-match-p org-agenda-file-regexp filename)
                                    (ignore-errors
                                      (akirak-org-reg--format-marker marker)))))
        (let ((symbol (intern (format "akirak-org-reg-dispatch-on-%c" key))))
          (unless (fboundp symbol)
            (fset symbol
                  `(lambda ()
                     (interactive)
                     (akirak-org-reg-dispatch (alist-get ,key register-alist))))
            (put symbol 'interactive-only t))
          (push `(,transient--default-child-level
                  transient-suffix
                  ,(list :key (char-to-string key)
                         :description (propertize description 'face 'font-lock-doc-face)
                         :command symbol
                         :transient nil))
                entries))))
    (nreverse entries)))

;;;; Prefix

;;;###autoload (autoload 'akirak-org-reg-transient "akirak-org-reg" nil 'interactive)
(transient-define-prefix akirak-org-reg-transient ()
  "Dispatch an action on a place."
  ["Dynamic"
   ("j" akirak-org-reg-dispatch-on-clock
    :transient nil)
   ("m" akirak-org-reg-dispatch-on-memento
    :transient nil)
   ("c" akirak-org-reg-dispatch-on-last-capture
    :transient nil)]
  ["Registry"
   :setup-children akirak-org-reg-register-suffixes]
  (interactive)
  (transient-setup 'akirak-org-reg-transient))

(defun akirak-org-reg-dispatch (target)
  (cl-etypecase target
    (symbol (akirak-org-reg-dispatch (symbol-value target)))
    (marker (funcall akirak-org-reg-action-fn target))))

(provide 'akirak-org-reg)
;;; akirak-org-reg.el ends here
