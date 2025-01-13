;;; akirak-files.el --- Extra utilities for files -*- lexical-binding: t -*-

(defvar akirak-files-home-symlink-alist nil)

;;;###autoload
(defun akirak-files-update-abbrev-alist ()
  "Update the value of `directory-abbrev-alist'."
  (interactive)
  (require 'project)
  (let (result)
    (cl-labels
        ((go (parent)
           (when (and (file-directory-p parent)
                      (not (project-try-vc parent)))
             (pcase-dolist (`(,path ,init . ,_)
                            (directory-files-and-attributes
                             parent
                             'full
                             (rx bol (not (any ".")))
                             'nosort))
               (pcase init
                 (`nil)
                 (`t (go path))
                 ;; Ignore paths to the Nix store or git-annex files
                 ((rx bol "/nix"))
                 ;; Absolute: Add
                 ((rx bol "/")
                  (push (cons init path) result))
                 ;; Relative: Resolve
                 ((pred stringp)
                  (push (cons (file-truename (expand-file-name init parent)) path)
                        result)))))))
      (go "~/"))
    (setq akirak-files-home-symlink-alist result)
    (setq directory-abbrev-alist
          (cl-merge 'list
                    directory-abbrev-alist
                    (mapcar (pcase-lambda (`(,from . ,to))
                              (cons (concat "\\`" (regexp-quote (file-name-as-directory from)))
                                    (file-name-as-directory to)))
                            result)
                    (lambda (x y)
                      (string-equal (car x) (car y)))))))

;;;###autoload
(defun akirak-files-ensure-abbrev-list ()
  "Ensure that `directory-abbrev-alist' is set."
  (unless akirak-files-home-symlink-alist
    (akirak-files-update-abbrev-alist)))

;;;###autoload
(defun akirak-files-set-dir-locals-class ()
  "Set the dir-locals class for a directory."
  (interactive)
  (let ((dir (read-file-name "Directory: " (vc-git-root default-directory)))
        (class (intern (completing-read "Class: "
                                        (thread-last
                                          (mapcar #'car dir-locals-class-alist)
                                          (seq-filter
                                           (lambda (sym)
                                             (not (string-prefix-p "/" (symbol-name sym))))))
                                        nil t))))
    (dir-locals-set-directory-class dir class)))

(provide 'akirak-files)
;;; akirak-files.el ends here
