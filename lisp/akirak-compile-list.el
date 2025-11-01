;;; akirak-compile-list.el --- Tablist interface for compile -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tablist)
(require 'akirak-compile)

(defconst akirak-compile-list--buffer-name
  "*Compile Cached Commands*")

;;;###autoload
(defun akirak-compile-list-cached-commands ()
  "Display cached compile commands in tablist."
  (interactive)
  (let ((buffer (get-buffer-create akirak-compile-list--buffer-name)))
    (with-current-buffer buffer
      (akirak-compile-list-mode))
    (pop-to-buffer buffer)))

(define-derived-mode akirak-compile-list-mode tabulated-list-mode
  "Akirak Compile Cache"
  (setq tabulated-list-format
        [("Directory" 30 nil)
         ("Backend" 8 nil)
         ("Command" 30 nil)
         ("Terminal" 5 nil)
         ("Running" 5 nil)
         ("Annotation" 35 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Backend" . nil)
        tabulated-list-entries #'akirak-compile-list--entries)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (tablist-minor-mode 1))

(defun akirak-compile-list--make-entry (backend dir entry)
  (let* ((command (akirak-compile-list--stringify (car entry)))
         (plist (cdr entry))
         (annotation (when plist
                       (akirak-compile-list--stringify
                        (plist-get plist 'annotation))))
         (terminal (and plist (plist-get plist 'terminal)))
         (display-dir (if dir (abbreviate-file-name dir) ""))
         (running nil))
    (list (list backend dir command)
          (vector display-dir
                  (symbol-name backend)
                  command
                  (if terminal "yes" "no")
                  running
                  annotation))))

(defun akirak-compile-list--format-extras (plist)
  (when plist
    (let (result)
      (while plist
        (let ((key (pop plist))
              (value (pop plist)))
          (unless (memq key '(annotation terminal))
            (push (if value
                      (format "%s=%s" key value)
                    (symbol-name key))
                  result))))
      (or (string-join (nreverse result) ", ") ""))))

(defun akirak-compile-list--entry< (a b)
  (cl-destructuring-bind (backend-a dir-a command-a) (car a)
    (cl-destructuring-bind (backend-b dir-b command-b) (car b)
      (let ((backend-a (symbol-name backend-a))
            (backend-b (symbol-name backend-b))
            (dir-a (or dir-a ""))
            (dir-b (or dir-b ""))
            (command-a (or command-a ""))
            (command-b (or command-b "")))
        (cond
         ((string< backend-a backend-b) t)
         ((string< backend-b backend-a) nil)
         ((string< dir-a dir-b) t)
         ((string< dir-b dir-a) nil)
         (t (string< command-a command-b)))))))

(defun akirak-compile-list--entries ()
  (let (rows)
    (maphash
     (lambda (key commands)
       (pcase key
         (`(,backend ,dir)
          (dolist (entry commands)
            (when entry
              (push (akirak-compile-list--make-entry backend dir entry)
                    rows))))))
     akirak-compile-command-cache)
    (sort rows #'akirak-compile-list--entry<)))

(defun akirak-compile-list--stringify (value)
  (if value (format "%s" value) ""))

(provide 'akirak-compile-list)
;;; akirak-compile-list.el ends here
