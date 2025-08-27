;;; akirak-imenu.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-imenu-run-test (&optional initial)
  "Run tests by selecting an imenu entry."
  (interactive)
  (pcase (if (eq major-mode 'imenu-list-major-mode)
             (with-current-buffer imenu-list--displayed-buffer
               (akirak-imenu--scan-tests))
           (akirak-imenu--scan-tests))
    (`(,language . ,entries)
     (let ((name (pcase (and initial
                             (seq-filter (lambda (candidate)
                                           (string-match-p (regexp-quote initial) candidate))
                                         entries))
                   (`(,candidate)
                    candidate)
                   (_
                    (completing-read "Test: " entries nil nil initial)))))
       (akirak-compile-run-test name language)))))

(defun akirak-imenu--scan-tests ()
  (pcase (derived-mode-p '(java-ts-mode))
    (`java-ts-mode
     (let* ((package (caar imenu--index-alist))
            (class (caadr imenu--index-alist))
            (class-qname (format "%s.%s" package class)))
       (cl-assert (equal (get-text-property 0 'breadcrumb-kind package) "Package"))
       (cl-assert (equal (get-text-property 0 'breadcrumb-kind class) "Class"))
       (cons 'java
             (cons class-qname
                   (thread-last
                     (cdadr imenu--index-alist)
                     (mapcar #'car)
                     (seq-filter (lambda (name)
                                   (equal (get-text-property 0 'breadcrumb-kind name)
                                          "Method")))
                     (mapcar `(lambda (name)
                                (format "%s.%s" ,class-qname
                                        (string-remove-suffix "()" name)))))))))))

(provide 'akirak-imenu)
;;; akirak-imenu.el ends here
