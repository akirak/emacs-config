;;; akirak-files.el --- Extra utilities for files -*- lexical-binding: t -*-

(defvar akirak-files-home-symlink-alist nil)

;;;###autoload
(defun akirak-files-update-abbrev-alist ()
  "Update the value of `directory-abbrev-alist'."
  (interactive)
  (let (result)
    (cl-labels
        ((go (parent)
             (when (file-directory-p parent)
               (pcase-dolist (`(,path ,init . ,_)
                              (directory-files-and-attributes
                               parent
                               'full
                               (rx bol (not (any ".")))
                               'nosort))
                 (pcase init
                   (`nil)
                   (`t (go path))
                   ;; Ignore paths to the Nix store
                   ((rx bol "/nix/"))
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

(provide 'akirak-files)
;;; akirak-files.el ends here
