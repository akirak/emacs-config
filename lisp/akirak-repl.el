;;; akirak-repl.el ---  -*- lexical-binding: t -*-

(defvar rtog/mode-repl-alist)

;;;###autoload
(defun akirak-repl (&optional arg)
  "Create or display a scratch buffer for the current project."
  (interactive "P")
  (pcase-exhaustive arg
    (`nil
     (let* ((buffer-name "*scratch elisp*")
            (buffer (get-buffer buffer-name)))
       (with-current-buffer (or buffer
                                (generate-new-buffer buffer-name))
         (unless buffer
           (lisp-interaction-mode)
           (insert ";; A scratch buffer for emacs-lisp.\n"))
         (pop-to-buffer (current-buffer)
                        '(display-buffer-below-selected . nil)))))
    (`(4)
     (let ((func (akirak-repl-complete "Run a REPL: ")))
       (if (commandp func)
           (call-interactively func)
         (funcall func))))))

(defun akirak-repl-complete (prompt)
  (require 'repl-toggle)
  (let ((candidates (thread-last
                      rtog/mode-repl-alist
                      (mapcar #'cdr)
                      (cl-remove-duplicates))))
    (cl-labels
        ((completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'command)))
             (complete-with-action action candidates string pred))))
      (intern (completing-read prompt #'completions
                               nil t)))))

(provide 'akirak-repl)
;;; akirak-repl.el ends here
