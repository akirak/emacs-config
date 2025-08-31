;;; akirak-completing.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-completing-read-no-sorted (&rest args)
  (let ((completions-sort nil))
    (apply #'completing-read args)))

(provide 'akirak-completing)
;;; akirak-completing.el ends here
