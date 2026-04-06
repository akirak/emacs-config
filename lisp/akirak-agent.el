;;; akirak-agent.el --- Common infrastructure for agents -*- lexical-binding: t -*-

;; Copyright (C) 2026 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides generic support for coding agents.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom akirak-agent-skills-dir ".agents/skills"
  "Default relative path for a project-local skill directory."
  :type '(choice string
                 (repeat string))
  :group 'akirak)

(defcustom akirak-agent-global-skills-dir "~/.agents/skills/"
  ""
  :type 'directory
  :group 'akirak)

(defun akirak-agent-parse-yaml-header ()
  "Parse YAML front matter in the current buffer into an alist.

The function expects the buffer to begin with a Markdown-style front
matter block delimited by lines containing only `---'.  Each non-empty
line inside the block must be a simple `key: value' pair.  The returned
alist uses interned symbols as keys."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "\\`---[ \t]*\n\\(\\(?:.*\n\\)*?\\)---[ \t]*\\(?:\n\\|\\'\\)"
           nil t)
      (let (alist)
        (dolist (line (split-string (match-string 1) "\n" t))
          (when (string-match "\\`\\([^:#[:space:]][^:]*\\):[ \t]*\\(.*\\)\\'" line)
            (push (cons (intern (match-string 1 line))
                        (match-string 2 line))
                  alist)))
        (nreverse alist)))))

(defun akirak-agent--skill-metadata (skills-dir)
  "Return metadata for a skill in SKILL-DIR."
  (let ((skill-file (file-name-concat skills-dir "SKILL.md")))
    (when (file-readable-p skill-file)
      (with-temp-buffer
        (insert-file-contents skill-file)
        (akirak-agent-parse-yaml-header)))))

(cl-defun akirak-agent--skills (root-dir &optional skills-dir
                                         &key extra-skills-dir)
  "Return skills in PROJECT-ROOT using CONVENTIONS.

Each element is of the form (NAME . METADATA)."
  (cl-assert (string-prefix-p (expand-file-name (file-name-as-directory root-dir))
                              (expand-file-name default-directory))
             nil
             "ROOT-DIR must be an ancestor of default-directory or self")
  (let ((skills-dirs (ensure-list (or skills-dir akirak-agent-skills-dir)))
        (cwd default-directory)
        (extra-skills-dir (or extra-skills-dir
                              akirak-agent-global-skills-dir))
        result)
    (while (not (file-equal-p cwd root-dir))
      (dolist (absdir (mapcar `(lambda (relative)
                                 (file-name-concat ,cwd relative))
                              skills-dirs))
        (when (file-directory-p absdir)
          (setq result (append result (akirak-agent--skills-in-dir absdir)))))
      (setq cwd (file-name-parent-directory cwd)))
    (when (and extra-skills-dir
               (file-directory-p extra-skills-dir))
      (setq result (append result (akirak-agent--skills-in-dir extra-skills-dir))))
    result))

(defun akirak-agent--skills-in-dir (skills-dir)
  (let (results)
    (dolist (skill-dir (directory-files skills-dir 'full "^[A-Za-z0-9]" 'nosort))
      (when (file-exists-p (file-name-concat skill-dir "SKILL.md"))
        (push (let* ((metadata (akirak-agent--skill-metadata skill-dir))
                     (name (or (alist-get 'name metadata)
                               (file-name-base skill-dir))))
                (cons name metadata))
              results)))
    results))

(cl-defun akirak-agent-complete-skill (prompt &optional root-dir &key skills-dir)
  "Complete a skill name in PROJECT-ROOT with PROMPT.

CONVENTIONS controls the relative path to the skills directory.  It may
be nil, a relative path string, or a list of relative path components."
  (let* ((root-dir (or root-dir (vc-git-root default-directory)))
         (skills (akirak-agent--skills root-dir skills-dir))
         (candidates (mapcar #'car skills)))
    (unless candidates
      (user-error "No skills found in %s" root-dir))
    (cl-labels
        ((annotator (candidate)
           (if-let* ((metadata (assoc candidate skills))
                     (description (alist-get 'description (cdr metadata))))
               (concat " " description)
             ""))
         (completion (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'agent-skill)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (completing-read prompt #'completion nil t))))

(provide 'akirak-agent)
;;; akirak-agent.el ends here
