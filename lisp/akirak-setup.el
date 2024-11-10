;;; akirak-setup.el ---  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'setup)
  (require 'cl-lib))

;; This function was pasted from <https://www.emacswiki.org/emacs/SetupEl#h5o-4>
;; which was originally named `defsetup'.
(defmacro define-setup-macro (name signature &rest body)
  "Shorthand for `setup-define'.
  NAME is the name of the local macro.  SIGNATURE is used as the
  argument list for FN.  If BODY starts with a string, use this as
  the value for :documentation.  Any following keywords are passed
  as OPTS to `setup-define'."
  (declare (debug defun))
  (let (opts)
    (when (stringp (car body))
      (setq opts (nconc (list :documentation (pop body))
                        opts)))
    (while (keywordp (car body))
      (let* ((prop (pop body))
             (val `',(pop body)))
        (setq opts (nconc (list prop val) opts))))
    `(setup-define ,name
       (cl-function (lambda ,signature ,@body))
       ,@opts)))

(provide 'akirak-setup)
;;; akirak-setup.el ends here
