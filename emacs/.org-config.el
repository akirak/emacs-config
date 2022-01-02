;; This configuration is written for org-starter:
;; <https://github.com/akirak/org-starter>

;; <https://github.com/akirak/trivial-elisps>
(require 'akirak-org)
(require 'akirak-org-capture)

(org-starter-def "emacs-config.org"
  :key "e"
  :refile (:maxlevel . 4)
  :minor-modes (org-edna-mode
                akirak-org-sort-buffer-mode))

(akirak-org-capture-add-templates
    (cl-macrolet
        ((level2 (text)
                 `(lambda ()
                    (widen)
                    (goto-char (point-min))
                    (or (re-search-forward (rx bol "**" (+ space)
                                               ,(regexp-quote text))
                                           nil t)
                        (error "Heading \"%s\" is not found in the file" text)))))
      (let ((file (org-starter-locate-file "emacs-config.org" nil t)))
        (doct `(("Emacs Config" :keys "e"
                 :when ,(stringp file)
                 :file ,file
                 :template
                 ("* %^{Name}"
                  ,akirak-org-capture-default-drawer
                  "#+begin_src emacs-lisp"
                  "%{src}"
                  "#+end_src")
                 :children
                 (("Builtin" :keys "b"
                   :function ,(level2 "Built-ins")
                   :src "(setup %\\1\n  %?)")
                  ("Org package" :keys "o"
                   :function ,(level2 "Org")
                   :src "(setup (:package %\\1)%?)")
                  ("Packages" :keys "p"
                   :function ,(level2 "Packages")
                   :src "(setup (:package %\\1)%?)")

                  ("Maybe" :keys "m"
                   :template
                   ("* MAYBE %^{Name} :noexport:"
                    ,akirak-org-capture-default-drawer
                    "%?")
                   :children
                   (("Org package" :keys "o"
                     :function ,(level2 "Org"))
                    ("Packages" :keys "p"
                     :function ,(level2 "Packages"))))

                  ("Notes" :keys "n"
                   :function ,(level2 "Notes")
                   :template
                   ("* %?"
                    ,akirak-org-capture-default-drawer))

                  ("Source block for a package" :keys "s"
                   :contexts (:in-file "emacs-config\\.org\\'")
                   :type plain
                   :template
                   (lambda ()
                     (concat "#+begin_src emacs-lisp\n"
                             "(setup (:package "
                             (substring-no-properties
                              (org-get-heading t t t t))
                             ")%?)\n"
                             "#+end_src"))))))))))
