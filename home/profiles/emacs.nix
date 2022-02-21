{
  programs.emacs-unsafe = {
    enable = true;

    extraInitText = ''
      (require 'sanityinc-tomorrow-eighties-theme)
      (load-theme 'sanityinc-tomorrow-eighties t)

      (eval-after-load 'org
        (defun akirak/setup-org-starter ()
          (unless (bound-and-true-p org-starter-path)
            (setq org-starter-path
                  (thread-last (directory-files-and-attributes "~/" t "^[a-z]")
                    (cl-remove-if-not
                     (pcase-lambda (`(,path ,is-dir . ,_))
                       (and is-dir
                            (not (equal (file-name-base path) "result"))
                            (file-exists-p (expand-file-name ".org-config.el" path)))))
                    (mapcar
                     (lambda (ent) (concat (abbreviate-file-name (car ent)) "/"))))))
          (when (member "~/config/" org-starter-path)
            (add-to-list 'org-starter-path "~/config/emacs/"))
          (require 'org-starter)
          (org-starter-mode t)))
    '';

    extraDirsToTryBind = [
      "/run/user/$(id -u)"
    ];
  };
}
