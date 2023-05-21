;;; akirak-cachix.el --- Cachix wrapper -*- lexical-binding: t -*-

(defcustom akirak-cachix-name "akirak"
  ""
  :type 'string)

(defcustom akirak-cachix-executable "cachix"
  ""
  :type 'file)

;;;###autoload
(defun akirak-cachix-push (files)
  "Push a file to cachix."
  (interactive (list (file-truename
                      (read-file-name
                       "File to push: "
                       (locate-dominating-file default-directory "flake.nix")
                       nil
                       t
                       "result"))))
  (compile (mapconcat #'shell-quote-argument
                      (append (list akirak-cachix-executable
                                    "push"
                                    akirak-cachix-name)
                              (ensure-list files))
                      " ")))

(provide 'akirak-cachix)
;;; akirak-cachix.el ends here
