;; Rather than using `with-eval-after-load', using `eval-after-load' and giving
;; an explicit function name prevents duplicate addition of entries for the same
;; project.
(setq akirak/my-nixos-config-directory
      (file-name-directory (or load-file-name (buffer-file-name))))

(defmacro akirak/define-nix-run (name package &optional args)
  (declare (indent 1))
  `(defun ,(intern (concat "akirak/" name)) (args)
     (interactive (list (if current-prefix-arg
                            (read-string "Arguments: " ,args)
                          ,args)))
     (let ((default-directory (project-root (project-current))))
       (compile (concat "nix run .#" ,package
                        " --print-build-logs"
                        (if args (concat " " args) ""))))))

(eval-after-load 'project-hercules
  (defun akirak-project-hercules-define-config ()
    (let ((map (project-hercules-make-map akirak/my-nixos-config-directory
                                          :transient t)))
      (define-key map (kbd "E")
        (akirak/define-nix-run "run-emacs"
          "emacs-personalized" "--impure"))
      (define-key map (kbd "L")
        (akirak/define-nix-run "lock-elisp-packages"
          "lock" "--impure"))
      (define-key map (kbd "U")
        (akirak/define-nix-run "update-elisp-packages"
          "update-elisp")))))
