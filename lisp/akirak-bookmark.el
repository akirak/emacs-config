;;; akirak-bookmark.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;; I ended up not using this library. It is deprecated.

;;; Code:

(require 'bookmark)

(defcustom akirak-bookmark-static-property-alist
  '((org-placeholder t nested))
  ""
  :type '(alist :key-type symbol
                :value (repeat sexp)))

;;;###autoload
(defun akirak-bookmark-alter-property (bookmark)
  (interactive (list (bookmark-completing-read "Bookmark name: ")))
  (bookmark-maybe-load-default-file)
  (let* ((current (bookmark-get-bookmark-record bookmark))
         (property (akirak-bookmark--complete-property
                    "Bookmark property: "
                    (mapcar #'car current)))
         (default (cdr (assq property current)))
         (static-values (cdr (assq property akirak-bookmark-static-property-alist)))
         (value (if static-values
                    (read (completing-read (format-prompt "Value" default)
                                           (mapcar #'prin1-to-string static-values)
                                           nil nil nil nil default))
                  (read-from-minibuffer (format-prompt "Value" default)
                                        nil nil #'read nil
                                        (prin1-to-string default)))))
    (bookmark-prop-set bookmark property value)
    (bookmark-save)))

(defun akirak-bookmark--complete-property (prompt &optional properties)
  (let ((candidates (append properties
                            (thread-last
                              bookmark-alist
                              (mapcar #'cdr)
                              (apply #'append)
                              (mapcar #'car)
                              (cl-remove-duplicates)))))
    (cl-labels
        ((group (candidate transform)
           (if transform
               candidate
             (if (memq (intern candidate) properties)
                 "Existing properties"
               "Other properties")))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'bookmark)
                           (cons 'group-function #'group)))
             (complete-with-action action candidates string pred))))
      (let ((completions-sort nil))
        (intern-soft (completing-read prompt #'completions))))))

(provide 'akirak-bookmark)
;;; akirak-bookmark.el ends here
