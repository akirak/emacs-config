;;; akirak-org-reg.el --- Transient for Org places -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'org)
(require 'org-capture)
(require 'transient)
(require 'akirak-transient)

(defcustom akirak-org-reg-action-fn #'akirak-embark-on-org-headline
  "Default function called on an Org marker."
  :type 'function)

(defcustom akirak-org-reg-action-fn-alist
  '(("item" .  akirak-embark-on-org-item))
  "Function called on an Org marker."
  :type '(alist :key-type string :value-type function))

;;;; Infixes

(defvar akirak-org-reg-target-type nil)

(transient-define-infix akirak-org-reg-switch-target ()
  :description "Change target type"
  :class 'akirak-transient-choice-variable
  :choices '("item"
             "heading")
  :variable 'akirak-org-reg-target-type)

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
  (if (and (markerp marker)
           (buffer-live-p (marker-buffer marker)))
      (format "\"%s\" in %s"
              (org-link-display-format (org-entry-get marker "ITEM"))
              (buffer-name (marker-buffer marker)))
    "(buffer not live)"))

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

;;;;; Indirect buffers

(defclass akirak-org-reg-completion-suffix (transient-variable)
  ((format :initform " %k %d")))

(cl-defmethod transient-format ((obj akirak-org-reg-completion-suffix))
  (format-spec (oref obj format)
               `((?k . ,(transient-format-key obj))
                 (?d . ,(transient-format-description obj)))))

(transient-define-suffix akirak-org-reg-dispatch-on-indirect-entry ()
  :class 'akirak-org-reg-completion-suffix
  :description "Indirect buffer entries"
  (interactive)
  (cl-flet
      ((buffer-org-mode-p (buffer)
         (provided-mode-derived-p (buffer-local-value 'major-mode buffer)
                                  'org-mode))
       (buffer-entry-cell (buffer)
         (with-current-buffer buffer
           (cons (org-entry-get (point-min) "ITEM")
                 (copy-marker (point-min))))))
    (let* ((entries (thread-last
                      (buffer-list)
                      (seq-filter #'buffer-base-buffer)
                      (seq-filter #'buffer-org-mode-p)
                      (mapcar #'buffer-entry-cell)))
           (name (completing-read "Org entry in indirect buffer: " entries nil t)))
      (akirak-org-reg-dispatch (cdr (assoc name entries))))))

;;;; Prefix

;;;###autoload (autoload 'akirak-org-reg-transient "akirak-org-reg" nil 'interactive)
(transient-define-prefix akirak-org-reg-transient ()
  "Dispatch an action on a place."
  ["Dispatch embark action on an org-mode target"
   ("-t" akirak-org-reg-switch-target)]
  ["Dynamic"
   ("j" akirak-org-reg-dispatch-on-clock
    :transient nil)
   ("m" akirak-org-reg-dispatch-on-memento
    :transient nil)
   ("c" akirak-org-reg-dispatch-on-last-capture
    :transient nil)
   ("i" akirak-org-reg-dispatch-on-indirect-entry
    :transient nil)]
  ["Registry"
   :setup-children akirak-org-reg-register-suffixes]
  (interactive)
  (transient-setup 'akirak-org-reg-transient))

(defun akirak-org-reg-dispatch (target)
  (cl-etypecase target
    (symbol (akirak-org-reg-dispatch (symbol-value target)))
    (marker
     (if-let* ((fn (cdr (assoc akirak-org-reg-target-type akirak-org-reg-action-fn-alist))))
         (funcall fn target)
       (funcall akirak-org-reg-action-fn target)))))

(provide 'akirak-org-reg)
;;; akirak-org-reg.el ends here
