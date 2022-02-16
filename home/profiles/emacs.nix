{
  programs.emacs-unsafe = {
    enable = true;

    extraInitText = ''
      (require 'sanityinc-tomorrow-eighties-theme)
      (load-theme 'sanityinc-tomorrow-eighties t)

      (add-hook 'after-init-hook
                (lambda ()
                  (require 'org-starter)
                  (pcase-dolist
                        (`(,path ,is-dir . ,_)
                    (directory-files-and-attributes "~/" t "^[^.]"))
                    (when (and is-dir
                       (file-exists-p (expand-file-name ".org-config.el" path)))
                       (push (concat (abbreviate-file-name path) "/")
                             org-starter-path)))
                  (org-starter-mode t)))
    '';
  };
}
