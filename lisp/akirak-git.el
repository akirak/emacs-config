;;; akirak-git.el ---  -*- lexical-binding: t -*-

;;;###autoload
(defun akirak-git-list-remote-pages ()
  (interactive)
  (let ((url (completing-read "Browse git remote: "
                              (completion-table-with-metadata
                               (akirak-git--remote-urls)
                               '((category . url))))))
    (browse-url url)))

(defun akirak-git--remote-urls ()
  (thread-last
    (process-lines "git" "config" "--list" "--local")
    (mapcar (lambda (line)
              (when (string-match (rx bol "remote." (+ (any "-" alnum)) ".url="
                                      (group (+ anything)))
                                  line)
                (match-string 1 line))))
    (delq nil)
    (mapcar #'akirak-git--git-html-url)))

(defun akirak-git--git-html-url (git-url)
  "Convert GIT-URL to an HTML url."
  (thread-last
    (pcase git-url
      ((rx bol "git@" (group "github.com") ":" (group (+ anything)))
       (concat "https://" (match-string 1 git-url) "/"
               (match-string 2 git-url)))
      (_
       git-url))
    (string-remove-suffix ".git")))

(provide 'akirak-git)
;;; akirak-git.el ends here
