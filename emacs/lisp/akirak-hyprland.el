;;; akirak-hyprland.el ---  -*- lexical-binding: t -*-

(defcustom akirak-hyprland-hyprctl-executable
  "hyprctl"
  ""
  :type 'file)

(cl-defun akirak-hyprland-read-window (prompt)
  (let ((alist (thread-last
                 (akirak-hyprland--hyprctl "clients")
                 (mapcar (lambda (x)
                           (cons (alist-get 'title x)
                                 x))))))
    (cl-labels
        ((annotator (candidate)
           (thread-last
             (assoc candidate alist)
             (cdr)
             (alist-get 'class)
             (concat " ")))
         (group (candidate transform)
           (when transform
             (thread-last
               (assoc candidate alist)
               (cdr)
               (alist-get 'workspace)
               (alist-get 'name))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'hyprland-window)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (completing-read prompt #'completions nil t))))

(defun akirak-hyprland--hyprctl (command)
  (with-temp-buffer
    (unless (zerop (call-process akirak-hyprland-hyprctl-executable nil (list t nil) nil
                                 command "-j"))
      (error "hyprctl returned non-zero"))
    (goto-char (point-min))
    (json-parse-buffer :array-type 'list :object-type 'alist)))

(provide 'akirak-hyprland)
;;; akirak-hyprland.el ends here
