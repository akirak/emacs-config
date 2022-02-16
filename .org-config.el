;; Rather than using `with-eval-after-load', using `eval-after-load' and giving
;; an explicit function name prevents duplicate addition of entries for the same
;; project.
(setq akirak/my-nixos-config-directory
      (file-name-directory (or load-file-name (buffer-file-name))))

(defun akirak/nixos-rebuild (command)
  (compile (format "sudo nixos-rebuild %s --flake %s#%s"
                   command
                   (thread-last akirak/my-nixos-config-directory
                     (file-truename)
                     (string-remove-suffix "/"))
                   (car (process-lines "uname" "-n")))))

(eval-after-load 'project-hercules
  (defun akirak-project-hercules-define-config ()
    (let ((map (project-hercules-make-map akirak/my-nixos-config-directory
                                          :transient t)))
      (define-key map (kbd "B")
        (defun akirak/nixos-rebuild-build ()
          "Run nixos-rebuild build."
          (interactive)
          (akirak/nixos-rebuild "build")))
      (define-key map (kbd "R")
        (defun akirak/nixos-rebuild-switch ()
          "Run nixos-rebuild switch."
          (interactive)
          (akirak/nixos-rebuild "switch"))))))
