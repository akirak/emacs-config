;;; akirak-elisp.el --- Miscellaneous functions for emacs-lisp-mode -*- lexical-binding: t -*-

(defcustom akirak-elisp-let-templates
  '(("let"
     ("(let ((" (p "Name: ") " " r "))" n> p ")"))
    ("let*"
     ("(let* ((" (p "Name: ") " " r "))" n> p ")"))
    ("when-let*"
     ("(when-let* ((" (p "Name: ") " " r "))" n> p ")"))
    ("if-let*"
     ("(if-let* ((" (p "Name: ") " " r "))" n> p ")"))
    ("when-let"
     ("(when-let (" (p "Name: ") " " r ")" n> p ")"))
    ("if-let"
     ("(if-let (" (p "Name: ") " " r ")" n> p ")")))
  ""
  :type '(repeat (list (string :tag "Macro/function")
                       (sexp :tag "Tempo elements"))))

(defcustom akirak-elisp-wrap-templates
  '(("let"
     ("(let (" p ")" n> r ")"))
    ("let*"
     ("(let* (" p ")" n> r ")"))
    ("if-let"
     ("(if-let (" p ")" n> r ")"))
    ("when-let"
     ("(when-let (" p ")" n> r ")")))
  ""
  :type '(repeat (list (string :tag "Macro/function")
                       (sexp :tag "Tempo elements"))))

;;;###autoload
(defun akirak-elisp-let-exp (type)
  (interactive (list (completing-read "Bind the expression in a form: "
                                      akirak-elisp-let-templates
                                      nil t)))
  (let ((template (cadr (assoc type akirak-elisp-let-templates))))
    (akirak-elisp--run-tempo template t)))

;;;###autoload
(defun akirak-elisp-wrap-exp (type)
  (interactive (list (completing-read "Wrap the expression with a form: "
                                      (akirak-elisp--callables)
                                      nil t)))
  (if-let (template (cadr (assoc type akirak-elisp-wrap-templates)))
      (akirak-elisp--run-tempo template t)
    (let ((arity (func-arity (symbol-function (intern type)))))
      (akirak-elisp--run-tempo
       `("(" ,type
         ,@(unless (and (numberp (car arity))
                        (= 0 (car arity)))
             '(" "))
         n> r p
         ")")
       t))))

(defun akirak-elisp--callables ()
  (let (result)
    (cl-do-all-symbols (sym)
      (when-let (func (fboundp sym))
        (push (symbol-name sym) result)))
    result))

(defun akirak-elisp--run-tempo (elements on-region)
  (when on-region
    (unless (use-region-p)
      (push-mark)
      (forward-sexp)
      (activate-mark)))
  (let ((sym (gensym "akirak-elisp-template")))
    (set sym elements)
    (unwind-protect
        (tempo-insert-template sym on-region)
      (unintern (symbol-name sym) obarray))))

(provide 'akirak-elisp)
;;; akirak-elisp.el ends here
