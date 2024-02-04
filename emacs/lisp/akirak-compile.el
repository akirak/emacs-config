;;; akirak-compile.el ---  -*- lexical-binding: t -*-

(defcustom akirak-compile-package-file-alist
  '(("dune-project" . dune))
  ""
  :type '(alist :key-type (string :tag "File name")
                :value-type (symbol :tag "Symbol to denote the project type")))

;;;###autoload
(defun akirak-compile ()
  (interactive)
  (if-let (workspace (vc-git-root default-directory))
      (pcase (akirak-compile--complete
              (akirak-compile--find-projects (expand-file-name workspace)))
        (`(,command . ,dir)
         (let ((default-directory dir))
           (compile command)))
        ((and command
              (pred stringp))
         (let ((default-directory workspace))
           (compile command)))))
  (user-error "No VC root"))

(defun akirak-compile--root ()
  (if-let (workspace (vc-git-root default-directory))
      (akirak-compile--find-projects (expand-file-name workspace))
    (user-error "No VC root")))

(defun akirak-compile--parent-dir (dir)
  (thread-last
    dir
    (string-remove-suffix "/")
    (file-name-directory)
    (file-name-as-directory)))

(defun akirak-compile--find-projects (workspace)
  (let* ((start (expand-file-name default-directory))
         result)
    (unless (string-prefix-p workspace start)
      (error "Directory %s is not a prefix of %s" workspace start))
    (cl-labels
        ((search (dir)
           (dolist (file (directory-files dir))
             (when-let (cell (assoc file akirak-compile-package-file-alist))
               (push (cons (cdr cell)
                           dir)
                     result)))
           (unless (equal (file-name-as-directory workspace)
                          (file-name-as-directory dir))
             (search (akirak-compile--parent-dir dir)))))
      (search start))
    result))

(defun akirak-compile--complete (projects)
  "Return (command . dir) or command for the next action for PROJECTS."
  (let (candidates
        (group-table (make-hash-table :test #'equal))
        (dir-table (make-hash-table :test #'equal))
        (ann-table (make-hash-table :test #'equal)))
    (pcase-dolist (`(,backend . ,dir) projects)
      (let ((command-alist (akirak-compile--gen-commands backend dir))
            (group (format "%s (%s)" backend (abbreviate-file-name dir))))
        (setq candidates (append candidates (mapcar #'car command-alist)))
        (pcase-dolist (`(,command . ,ann) command-alist)
          (when ann
            (puthash command ann ann-table))
          (puthash command dir dir-table)
          (puthash command group group-table))))
    (cl-labels
        ((annotator (candidate)
           (gethash candidate ann-table))
         (group (candidate transform)
           (if transform
               candidate
             (gethash candidate group-table)))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'command)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (let* ((input (completing-read "Compile: " #'completions))
             (dir (gethash input dir-table)))
        (if dir
            (cons input dir)
          input)))))

(defun akirak-compile--gen-commands (backend _dir)
  (pcase backend
    (`dune
     '(("dune build")
       ("dune exec")
       ("opam install ")))))

(provide 'akirak-compile)
;;; akirak-compile.el ends here
