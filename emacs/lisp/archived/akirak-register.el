;;; akirak-register.el ---  -*- lexical-binding: t -*-

(defun akirak-register-dwim (reg &optional arg)
  (interactive (list (register-read-with-preview "Dwim: ")
                     current-prefix-arg))
  (if-let (val (get-register reg))
      (pcase val
        ((cl-type marker)
         (register-val-jump-to val arg))
        ((cl-type string)
         (insert-register reg))
        ((and `(,x . ,_)
              (guard (window-configuration-p x)
                     (frame-configuration-p x)))
         (register-val-jump-to val arg))
        ((or `(file . ,_)
             `(buffer . ,_)
             `(file-query . ,_))
         (register-val-jump-to val arg))
        (_
         (error "Unknown action on register: "
                (register-val-describe val nil))))
    (error "No register entry at %s" reg)))

(provide 'akirak-register)
;;; akirak-register.el ends here
