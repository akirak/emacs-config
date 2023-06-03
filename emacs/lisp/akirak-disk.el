;;; akirak-disk.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-disk-findmnt ()
  "Visit a mountpoint"
  (interactive)
  (find-file (akirak-disk-read-findmnt "Mount point: ")))

(defun akirak-disk-read-findmnt (prompt)
  (let* ((alist (thread-last
                  (akirak-disk--findmnt-run "--real")
                  (assq 'filesystems)
                  (cdr)
                  (mapcar (lambda (x)
                            (cons (file-name-as-directory
                                   (cdr (assq 'target x)))
                                  x)))))
         (candidates (mapcar #'car alist)))
    (cl-labels
        ((annotator (candidate)
           (thread-last
             (assoc candidate alist)
             (cdr)
             (assq 'source)
             (cdr)
             (concat " ")))
         (group (candidate transform)
           (if transform
               candidate
             (thread-last
               (assoc candidate alist)
               (cdr)
               (assq 'fstype)
               (cdr))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'mountpoint)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (completing-read prompt #'completions
                       nil t))))

(defun akirak-disk--findmnt-run (&rest args)
  (with-temp-buffer
    (apply #'call-process
           "findmnt" nil (list t nil) nil
           "--json"
           args)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

(provide 'akirak-disk)
;;; akirak-disk.el ends here
