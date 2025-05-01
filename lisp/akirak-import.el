;;; akirak-import.el --- Import dependencies quickly -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Akira Komamura

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

;;; Code:


(require 'pcase)
(require 'map)

(defcustom akirak-import-settings-alist
  `((gleam-ts-mode
     :regexp ,(rx bol "import " (+ nonl))
     :extensions (".gleam")
     :source-directories ("src")
     :transform-filename
     (lambda (filepath identifier)
       (concat "import " (file-name-sans-extension filepath)
               (when identifier
                 (format ".{%s}"
                         (if (char-uppercase-p (aref identifier 0))
                             (concat "type " identifier)
                           identifier))))))
    ((typescript-ts-mode tsx-ts-mode)
     :regexp ,(rx bol "import " (+ nonl))
     :extensions (".ts" ".tsx")
     :source-directories ("src" "app")
     :package-file "package.json"
     :fixup-function akirak-import--typescript-fixup
     :transform-filename
     (lambda (filepath identifier)
       (let ((source (concat "\""
                             (akirak-import-typescript-encode-path filepath)
                             "\"")))
         (when (and identifier
                    (not (string-empty-p identifier)))
           (list (format "import { %s } from %s" identifier source)
                 (format "import %s from %s" identifier source))))))
    (elixir-ts-mode
     :regexp ,(rx bol (* blank) (or "alias" "import " "require" "use"))
     :treesit-node-types ("call")
     :extensions (".ex")
     :source-directories ("lib")
     :goto-insert-location akirak-import--elixir-insert-location
     :make-default
     (lambda (identifier candidates)
       (when (char-uppercase-p (elt identifier 0))
         (seq-find `(lambda (s)
                      (and (string-match-p "^alias " s)
                           (string-match-p (concat (rx (any "."))
                                                   (regexp-quote ,identifier)
                                                   "\\'")
                                           s)))
                   candidates)))
     :transform-filename
     (lambda (filepath identifier)
       (let ((module (akirak-elixir-module-name-from-file filepath)))
         (if identifier
             (format "import %s, only: [%s: n]" module identifier)
           (list (concat "alias " module)
                 (concat "import " module)
                 (concat "use " module)))))))
  ""
  :type '(alist :key-type (symbol :tag "Major mode")
                :value-type plist))

;;;###autoload
(defun akirak-import-thing-at-pt ()
  (interactive)
  (akirak-import-thing (thing-at-point 'symbol)))

(defun akirak-import-thing (&optional pattern)
  (if-let* ((entry (akirak-import--get-settings)))
      (pcase-exhaustive entry
        (`(,mode . ,(map :regexp :treesit-node-types :extra-modes
                         :extensions :source-directories :transform-filename
                         :goto-insert-location :fixup-function :package-file
                         :inside-tree-sitter-node :make-default))
         (let* ((existing-statements (akirak-import--collect-statements (ensure-list mode)
                                                                        :regexp regexp
                                                                        :treesit-node-types
                                                                        treesit-node-types))
                (generated-statements (akirak-import--generate-statements
                                       :identifier pattern
                                       :extensions extensions
                                       :package-file package-file
                                       :source-directories source-directories
                                       :transform-filename transform-filename))
                (lines (thread-last
                         (append generated-statements existing-statements)
                         (seq-sort #'string<)
                         (seq-uniq))))
           (cl-flet
               ((contains-pattern (s)
                  (let ((case-fold-search nil))
                    (string-match-p (regexp-quote pattern)
                                    s))))
             (akirak-import--insert-line
              (completing-read (if pattern
                                   (format-message "Insert an import statement for '%s': " pattern)
                                 "Insert an import statement: ")
                               lines
                               nil nil
                               (when (and pattern
                                          (seq-find #'contains-pattern lines))
                                 (concat pattern " "))
                               nil
                               (when (and make-default pattern)
                                 (funcall make-default pattern lines)))
              :goto-insert-location goto-insert-location
              :inside-tree-sitter-node inside-tree-sitter-node
              :fixup-function fixup-function
              :regexp regexp)))))
    (user-error "Unsupported mode")))

(defun akirak-import--get-settings ()
  (when-let* ((mode (apply #'derived-mode-p
                           (flatten-list (mapcar (lambda (cell)
                                                   (ensure-list (car cell)))
                                                 akirak-import-settings-alist)))))
    (seq-find `(lambda (cell)
                 (memq ',mode (ensure-list (car cell))))
              akirak-import-settings-alist)))

(cl-defun akirak-import--collect-statements (modes &key regexp treesit-node-types)
  "Collect import statements matching one of MODES."
  (let (lines)
    (cl-flet
        ((buffer-mode-p (buf)
           (memq (buffer-local-value 'major-mode buf)
                 modes)))
      (dolist (buffer (thread-last
                        (buffer-list)
                        (seq-filter #'buffer-mode-p)
                        (delete (current-buffer))))
        (with-current-buffer buffer
          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (while (re-search-forward regexp nil t)
                (if treesit-node-types
                    (let* ((start (match-beginning 0))
                           (node (treesit-node-at start)))
                      (while (not (member (treesit-node-type node)
                                          treesit-node-types))
                        (setq node (treesit-node-parent node))
                        (unless node
                          (error "No matching treesit node at %d" start)))
                      (push (treesit-node-text node) lines))
                  (push (string-trim (match-string 0)) lines))))))))
    lines))

(cl-defun akirak-import--generate-statements (&key identifier
                                                   extensions
                                                   source-directories
                                                   package-file
                                                   transform-filename)
  (let ((regexp (concat "/" (regexp-opt source-directories) "/")))
    (thread-last
      (akirak-consult--project-files 'absolute
                                     (when package-file
                                       (locate-dominating-file default-directory package-file)))
      (delete (buffer-file-name (buffer-base-buffer)))
      (mapcar `(lambda (filename)
                 (when (string-match ,regexp filename)
                   (let ((relpath (substring filename (match-end 0)))
                         (identifier ,identifier))
                     (funcall ',transform-filename
                              relpath
                              (when (and identifier
                                         (akirak-import--contains-identifier
                                          identifier filename))
                                identifier))))))
      (flatten-list))))

(defun akirak-import--contains-identifier (identifier file)
  (cl-labels
      ((predicate (item)
         (if (imenu--subalist-p item)
             (or (equal (car item) identifier)
                 (check-alist (cdr item)))
           (and (consp item)
                (equal (car item) identifier))))
       (check-alist (alist)
         (seq-some #'predicate alist)))
    (when-let* ((buffer (find-buffer-visiting file)))
      (let ((index (buffer-local-value 'imenu--index-alist buffer)))
        (if index
            (check-alist index)
          (with-current-buffer buffer
            (setq imenu--index-alist
                  (save-excursion
                    (without-restriction
                      (funcall imenu-create-index-function))))
            (check-alist imenu--index-alist)))))))

(cl-defun akirak-import--insert-line (content &key regexp inside-tree-sitter-node
                                              goto-insert-location fixup-function)
  ;; Save the position so the user can return the position being edited.
  (push-mark)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (let (pos)
        (cond
         ((and regexp
               (re-search-forward regexp nil t))
          (back-to-indentation)
          (open-line 1)
          (setq pos (point))
          (insert content))
         (goto-insert-location
          (funcall goto-insert-location)
          (setq pos (point))
          (insert content))
         (inside-tree-sitter-node)
         (t
          (open-line 1)
          (setq pos (point))
          (insert content)))
        (when pos
          (goto-char pos)
          (when (functionp fixup-function)
            (funcall fixup-function content)))))))

(defun akirak-import--elixir-insert-location ()
  (re-search-forward (rx bol "defmodule" (+ space)
                         (+ (any "." alnum))
                         (+ space) "do" symbol-end))
  (re-search-forward (rx (or "@moduledoc" alnum)))
  (if (equal (match-string 0) "@moduledoc")
      (progn
        (looking-at (rx (+ blank)))
        (goto-char (match-end 0))
        (if (looking-at "\"\"\"")
            (forward-sexp)
          (beginning-of-line 2))
        (newline-and-indent 2))
    (open-line 1)))

(defun akirak-import--typescript-fixup (&optional _content)
  (akirak-import--typescript-merge-statement))

(defun akirak-import--typescript-merge-statement ()
  "Merge the import statements of named imports from the same module."
  (cl-flet*
      ((get-nodes ()
         (thread-last
           (treesit-node-at (point))
           (treesit-node-parent)
           (treesit-node-children)))
       (node-types (nodes)
         (mapcar #'treesit-node-type nodes))
       (string-content (node)
         (pcase (treesit-node-children node)
           ((and `(,_open ,fragment ,_close)
                 (guard (equal (treesit-node-type fragment)
                               "string_fragment")))
            (treesit-node-text fragment))))
       (import-spec-p (node)
         (equal (treesit-node-type node)
                "import_specifier"))
       (named-imports-from-clause (node)
         (pcase (car (treesit-node-children node))
           ((and child
                 (guard (equal (treesit-node-type child) "named_imports")))
            (thread-last
              (treesit-node-children child)
              (cdr)
              (butlast)
              (seq-filter #'import-spec-p)
              (mapcar #'treesit-node-text)))))
       (get-named-imports (&optional nodes)
         (let ((nodes (or nodes (get-nodes))))
           (pcase (node-types nodes)
             ((and `("import" "import_clause" "from" "string")
                   (let imports (named-imports-from-clause (nth 1 nodes)))
                   (guard imports))
              (list (string-content (car (last nodes)))
                    imports))
             ((and `("import" "type" "import_clause" "from" "string")
                   (let imports (named-imports-from-clause (nth 2 nodes)))
                   (guard imports))
              (list (string-content (car (last nodes)))
                    imports))))))
    (pcase-let* ((new-nodes (get-nodes))
                 (`(,source-module ,new-imports) (get-named-imports new-nodes))
                 (bounds (cons (treesit-node-start (car new-nodes))
                               (treesit-node-end (car (last new-nodes))))))
      (goto-char (cdr bounds))
      (catch 'import-found
        (while (re-search-forward (rx bol "import") nil t)
          (let ((end (point)))
            (goto-char (match-beginning 0))
            (pcase (get-nodes)
              ((and nodes
                    (let `(,this-module ,imports) (get-named-imports nodes))
                    (guard (equal this-module source-module))
                    (let merged-imports (thread-last
                                          (append imports new-imports)
                                          (seq-uniq)))
                    (let quoted-source (treesit-node-text (car (last nodes)))))
               (delete-region (treesit-node-start (car nodes))
                              (treesit-node-end (car (last nodes))))
               (delete-region (car bounds) (cdr bounds))
               (insert (format "import { %s } from %s"
                               (string-join merged-imports ", ")
                               quoted-source))
               (goto-char (car bounds))
               (when (eolp)
                 (delete-char 1))
               (throw 'import-found t)))
            (goto-char end)))))))

(defcustom akirak-import-typescript-alias-config
  '("package.json"
    ("#*.js" . "./src/*.[tj]s"))
  ""
  :type '(cons (string :tag "Marker file name")
               (alist :key-type (string :tag "alias")
                      :value-type (string :tag "path"))))

(defun akirak-import-typescript-encode-path (path)
  (pcase akirak-import-typescript-alias-config
    (`(,marker . ,entries)
     (let* ((base-dir (locate-dominating-file default-directory marker))
            (relative-path (file-relative-name path base-dir)))
       (catch 'path-alias
         (pcase-dolist (`(,alias . ,path-pattern) entries)
           (when (string-match (akirak-import--glob-to-regexp path-pattern)
                               relative-path)
             (let ((param (match-string 1 relative-path)))
               (throw 'path-alias (if param
                                      (replace-regexp-in-string (rx "*") param alias)
                                    alias))))))))))

(defun akirak-import--glob-to-regexp (pattern)
  (with-temp-buffer
    (insert pattern)
    (goto-char (point-min))
    (when (looking-at (rx "./"))
      (replace-match "^")
      (goto-char (point-min)))
    (when (re-search-forward (rx "*") nil t)
      (replace-match "\\\\([^z-a]+\\\\)"))
    (buffer-string)))

;;;###autoload
(defun akirak-import-cleanup ()
  "Remove unnecessary imports in the current buffer."
  (interactive)
  (unless (bound-and-true-p eglot--managed-mode)
    (user-error "Only supported inside eglot-managed buffers"))
  (unless (bound-and-true-p flymake-mode)
    (user-error "Does not work without flymake-mode"))
  (pcase (or (akirak-import--get-settings)
             (user-error "Unsupported mode"))
    (`(,mode . ,(map :regexp))
     (save-restriction
       (widen)
       (save-excursion
         (goto-char (point-min))
         (let (bound
               (count 0))
           (while (re-search-forward regexp nil t)
             (setq bound (match-end 0)))
           (when bound
             (goto-char (point-min))
             (while-let ((pos (and (< (point) bound)
                                   (next-overlay-change (point)))))
               (goto-char pos)
               (pcase (catch 'unnecessary
                        (dolist (ov (overlays-at pos))
                          (when (and (overlay-get ov 'flymake-overlay)
                                     (memq 'eglot-diagnostic-tag-unnecessary-face
                                           (ensure-list (overlay-get ov 'face))))
                            (throw 'unnecessary (cons (overlay-start ov)
                                                      (overlay-end ov))))))
                 (`(,begin . ,end)
                  (delete-region begin end)
                  (when (looking-at (rx (* (any punctuation))
                                        (* (any blank))))
                    (delete-region (point) (match-end 0)))
                  (cl-incf count)
                  (goto-char (1- begin)))))
             (message "Removed %d imports" count))))))))

(provide 'akirak-import)
;;; akirak-import.el ends here
