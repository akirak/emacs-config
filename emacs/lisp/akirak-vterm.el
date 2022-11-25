;;; akirak-vterm.el --- Extra functions for vterm -*- lexical-binding: t -*-

(require 'vterm)
(require 'project)

(defvar workbox-default-directory)

;;;###autoload
(defun akirak-vterm-run-in-project (string)
  (interactive "sCommand: ")
  (if-let (pr (project-current))
      (akirak-vterm--run-in-dir (project-root pr) string)
    (user-error "Not in a project")))

;;;###autoload
(defun akirak-vterm-run-in-package-root (string)
  (interactive "sCommand: ")
  (akirak-vterm--run-in-dir workbox-default-directory string))

;;;###autoload
(defun akirak-vterm-run-in-cwd (string)
  (interactive "sCommand: ")
  (akirak-vterm--run-in-dir default-directory string))

(defun akirak-vterm--run-in-dir (dir string)
  (let* ((default-directory dir)
         (bufname (akirak-vterm--project-buffer-name dir string))
         (buffer (get-buffer bufname)))
    (if buffer
        (with-current-buffer buffer
          (vterm-send-string string)
          (vterm-send-return)
          (pop-to-buffer (current-buffer)))
      (with-current-buffer (save-window-excursion (vterm 'new))
        (rename-buffer bufname)
        (vterm-send-string string)
        (vterm-send-return)
        (pop-to-buffer (current-buffer))))))

(defun akirak-vterm--project-buffer-name (dir command)
  (format "*vterm:%s:%s*"
          (file-name-nondirectory (string-remove-suffix "/" dir))
          command))

(defun akirak-vterm--project-buffers (pr)
  (let ((prefix (format "*vterm:%s:"
                        (file-name-nondirectory
                         (string-remove-suffix "/" (project-root pr))))))
    (cl-flet
        ((project-vterm-p
           (buffer)
           (string-prefix-p prefix (buffer-name buffer))))
      (seq-filter #'project-vterm-p (buffer-list)))))

(defun akirak-vterm--command-identity (string)
  (let ((pos 0)
        (i 0)
        args)
    (catch 'finish
      (save-match-data
        (while (string-match (rx (+ (any "-+@:_.," alnum)))
                             string pos)
          (push (match-string 0 string) args)
          (when (> i 0)
            (throw 'finish t))
          (setq pos (match-end 0))
          (cl-incf i))))
    (string-join (nreverse args) " ")))

(defun akirak-vterm--buffers ()
  (cl-flet
      ((vterm-p
         (buffer)
         (eq (buffer-local-value 'major-mode buffer)
             'vterm-mode)))
    (seq-filter #'vterm-p (buffer-list))))

;;;###autoload
(defun akirak-vterm-for-project (&optional arg)
  (interactive "P")
  (let ((pr (project-current)))
    (if-let (buffer (cond
                     ((equal arg '(4))
                      (thread-last
                        (akirak-vterm--buffers)
                        (mapcar #'buffer-name)
                        (completing-read "Buffer: ")))
                     (pr
                      (car (akirak-vterm--project-buffers pr)))
                     (t
                      (car (akirak-vterm--buffers)))))
        (pop-to-buffer buffer)
      (if pr
          (akirak-vterm (project-root pr)
                        (akirak-vterm--project-buffer-name
                         (project-root pr) ""))
        (akirak-vterm)))))

;;;###autoload
(defun akirak-vterm-for-dir (&optional dir)
  "Pop up a vterm session dedicated to DIR."
  (interactive)
  (let* ((dir (or dir default-directory))
         (name "*vterm:%s*" (abbreviate-file-name dir)))
    (if-let (buffer (get-buffer name))
        (pop-to-buffer buffer)
      (akirak-vterm dir name))))

;;;###autoload
(defun akirak-vterm (&optional dir buffer-name)
  (interactive (pcase current-prefix-arg
                 (`nil (list default-directory))
                 ('(4) (list (read-directory-name "Directory: ")))))
  (let ((default-directory (or dir default-directory)))
    (with-current-buffer (save-window-excursion (vterm 'new))
      (when buffer-name
        (rename-buffer buffer-name))
      (pop-to-buffer (current-buffer)))))

(provide 'akirak-vterm)
;;; akirak-vterm.el ends here
