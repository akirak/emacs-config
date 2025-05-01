;;; akirak-github.el --- GitHub integration features -*- lexical-binding: t -*-

;; Copyright (C) 2025 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; URL: https://github.com/akirak/emacs-config
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This library defines the following commands:
;;
;; - `akirak-github-browse-issues-web`: Opens the issues page of the current
;;   GitHub repository in your web browser. You can optionally provide a search
;;   query to filter the issues.
;;
;; - `akirak-github-browse-prs-web`: Opens the pull requests page of the current
;;   GitHub repository in your web browser. You can optionally provide a search
;;   query to filter the pull requests.
;;
;; Both commands use the GitHub CLI (`gh`) to open the respective pages in the
;; browser. They are mostly intended to navigate issues on repositories
;; maintained by other people and organizations.

;;; Code:

(defcustom akirak-github-gh-executable "gh"
  ""
  :type 'file)

;;;###autoload
(defun akirak-github-browse-issues-web (&optional query)
  "Browse issues of the current GitHub repository online."
  (interactive
   (list (read-string "Query for GitHub issues (optional): ")))
  (apply #'start-process "gh" nil akirak-github-gh-executable
         "issue" "list" "--web"
         (when (and query
                    (not (string-empty-p query)))
           (list "--search" query))))

;;;###autoload
(defun akirak-github-browse-prs-web (&optional query)
  "Browse PRs of the current GitHub repository online."
  (interactive
   (list (read-string "Query for GitHub PRs (optional): ")))
  (apply #'start-process "gh" nil akirak-github-gh-executable
         "pr" "list" "--web"
         (when (and query
                    (not (string-empty-p query)))
           (list "--search" query))))

;;;###autoload
(transient-define-prefix akirak-github-workflow-run-transient ()
  ["View workflow"
   :class transient-row
   ("b" "Select a branch" akirak-github-view-workflow-on-branch)
   ("B" "This branch" akirak-github-view-workflow-on-this-branch)]
  (interactive)
  (transient-setup 'akirak-github-workflow-run-transient))

(cl-defun akirak-github-view-workflow (database-id &key url)
  (with-current-buffer (get-buffer-create (format "*gh run view<%s>*"
                                                  (project-name (project-current))))
    (widen)
    (erase-buffer)
    (message "Fetching the log for the workflow run...")
    (call-process akirak-github-gh-executable
                  nil t
                  nil
                  "run" "view" "--log" (int-to-string database-id))
    ;; Use tailspin for highlighting the log.
    ;;
    ;; TODO: Implement a better solution than tailspin
    (when (and (executable-find "tspin")
               (require 'xterm-color nil t))
      (call-process-region (point-min) (point-max) "tspin" nil (list t nil))
      (xterm-color-colorize-buffer))
    (goto-char (point-max))
    (when url
      (insert "\To browse the output on the web, visit " url))
    ;; Open the buffer in a tab. If there is an existing tab visiting the
    ;; buffer, switch to the tab.
    (unless (get-buffer-window (current-buffer))
      (pcase (tab-bar-get-buffer-tab (current-buffer))
        ;; (`(current-tab . ,_))
        (`(tab . ,alist)
         (tab-bar-switch-to-tab (alist-get 'name alist))
         (select-window (get-buffer-window (current-buffer))))
        (_
         (switch-to-buffer-other-tab (current-buffer)))))
    (let ((window (get-buffer-window (current-buffer))))
      (when (and window
                 (window-live-p window))
        (with-selected-window window
          (goto-char (point-max))
          (recenter-top-bottom -1))))))

(defun akirak-github-view-workflow-from-list (&rest gh-args)
  (apply #'akirak-github-view-workflow
         (apply #'akirak-github--select-workflow-run gh-args)))

(defun akirak-github-view-workflow-on-branch (branch)
  "Select a branch and view a workflow run on it."
  (interactive
   (list (completing-read "Select a branch: " (magit-remote-list-branches "origin")
                          nil t)))
  (akirak-github-view-workflow-from-list "--branch" branch))

(defun akirak-github-view-workflow-on-this-branch ()
  (interactive)
  (let ((upstream (magit-rev-parse "--abbrev-ref" "@{upstream}")))
    ;; Is there a better solution to remove the remote?
    (if (string-match (rx bol (+ alnum) "/") upstream)
        (akirak-github-view-workflow-on-branch (substring upstream (match-end 0)))
      (user-error "No upstream is set"))))

(defun akirak-github--select-workflow-run (&rest gh-args)
  (let* ((result
          (with-temp-buffer
            (message "Fetching workflow runs...")
            (apply #'call-process
                   akirak-github-gh-executable nil (list t nil) nil
                   "run" "list" "--json"
                   "displayTitle,status,updatedAt,workflowName,conclusion,event,headBranch,databaseId,url"
                   gh-args)
            (goto-char (point-min))
            (thread-last
              (json-parse-buffer :object-type 'alist :array-type 'list))))
         (alists (mapcar (lambda (alist)
                           (cons (alist-get 'databaseId alist)
                                 alist))
                         result))
         (candidates (mapcar (lambda (alist)
                               ;; TODO: Improve the style
                               (format-spec "%4i: %s %c (updated %u ago) %t / %w [%e,%b]"
                                            `((?i . ,(int-to-string (alist-get 'databaseId alist)))
                                              (?u . ,(ts-human-format-duration
                                                      (thread-last
                                                        (alist-get 'updatedAt alist)
                                                        ts-parse
                                                        (ts-diff (ts-now)))
                                                      'abbr))
                                              (?c . ,(alist-get 'conclusion alist))
                                              (?s . ,(alist-get 'status alist))
                                              (?b . ,(alist-get 'headBranch alist))
                                              (?e . ,(alist-get 'event alist))
                                              (?w . ,(alist-get 'workflowName alist))
                                              (?t . ,(alist-get 'displayTitle alist)))))
                             result)))
    (cl-labels
        ((completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'display-sort-function #'identity)))
             (complete-with-action action candidates string pred))))
      (let ((input (completing-read "Select a workflow run: " #'completions nil t)))
        (when (string-match (rx (+ digit)) input)
          (let* ((database-id (string-to-number (match-string 0 input)))
                 (alist (alist-get database-id alists)))
            (list database-id
                  :url (alist-get 'url alist))))))))

(provide 'akirak-github)
;;; akirak-github.el ends here
