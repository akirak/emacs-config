;;; akirak-quick-thing.el --- Quickly selecting an embark target -*- lexical-binding: t -*-

(require 'akirak-transient)
(require 'embark)

;;;###autoload (autoload 'akirak-quick-thing "akirak-quick-thing" nil 'interactive)
(transient-define-prefix akirak-quick-thing ()
  ["Embark on a target"
   :class transient-row
   :setup-children akirak-quick-thing-bindings-1]
  (interactive)
  (transient-setup 'akirak-quick-thing))

(defun akirak-quick-thing-bindings-1 (_children)
  (thread-last
    (embark--targets)
    (akirak-quick-thing--zip-index)
    (mapcar (pcase-lambda (`(,i . ,plist))
              (pcase-exhaustive plist
                ((map :type)
                 (let ((symbol (intern (format "akirak-quick-thing--embark-%d" i))))
                   (unless (fboundp symbol)
                     (fset symbol `(lambda () (interactive) (embark-act ,i)))
                     (put symbol 'interactive-only t))
                   `(,transient--default-child-level
                     transient-suffix
                     ,(list :key (substring (symbol-name type) 0 1)
                            :description (symbol-name type)
                            :command symbol)))))))
    (cl-remove-if #'null)))

(defun akirak-quick-thing--zip-index (list)
  (let ((i 0)
        x
        result)
    (while (setq x (pop list))
      (push (cons i x) result)
      (cl-incf i))
    (nreverse result)))

(provide 'akirak-quick-thing)
;;; akirak-quick-thing.el ends here
