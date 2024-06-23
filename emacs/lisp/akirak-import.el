;;; akirak-import.el --- Import dependencies quickly -*- lexical-binding: t -*-

(require 'pcase)
(require 'map)

(defcustom akirak-import-settings-alist
  `((gleam-ts-mode
     :regexp ,(rx bol "import " (+ nonl))
     :extra-modes nil
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
    (elixir-ts-mode
     :regexp ,(rx bol (* blank) (or "alias" "import " "require" "use") (+ nonl))
     :extra-modes nil
     :extensions (".ex")
     :source-directories ("lib")
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
  (pcase (assq (apply #'derived-mode-p (mapcar #'car akirak-import-settings-alist))
               akirak-import-settings-alist)
    (`nil
     (user-error "Unsupported mode"))
    (`(,mode . ,(map :regexp :extra-modes
                     :extensions :source-directories :transform-filename
                     :inside-tree-sitter-node))
     (let* ((existing-statements (akirak-import--collect-statements (cons mode extra-modes)
                                                                    :regexp regexp))
            (generated-statements (akirak-import--generate-statements
                                   :identifier pattern
                                   :extensions extensions
                                   :source-directories source-directories
                                   :transform-filename transform-filename))
            (lines (thread-last
                     (append generated-statements existing-statements)
                     (seq-sort #'string<))))
       (cl-flet
           ((contains-pattern (s)
              (let ((case-fold-search nil))
                (string-match-p (regexp-quote pattern)
                                s))))
         (akirak-import--insert-line
          (completing-read "Insert an import statement: "
                           lines
                           nil nil
                           (when (and pattern
                                      (seq-find #'contains-pattern lines))
                             (concat pattern " ")))
          :regexp regexp))))))

(cl-defun akirak-import--collect-statements (modes &key regexp)
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
                (push (match-string 0) lines)))))))
    lines))

(cl-defun akirak-import--generate-statements (&key identifier
                                                   extensions
                                                   source-directories
                                                   transform-filename)
  (let ((regexp (concat "/" (regexp-opt source-directories) "/")))
    (thread-last
      (akirak-consult--project-files 'absolute)
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
             (check-alist (cdr item))
           (and (consp item)
                (equal (car item) identifier))))
       (check-alist (alist)
         (seq-some #'predicate alist)))
    (when-let (buffer (find-buffer-visiting file))
      (let ((index (buffer-local-value 'imenu--index-alist buffer)))
        (if index
            (check-alist index)
          (with-current-buffer buffer
            (setq imenu--index-alist
                  (save-excursion
                    (without-restriction
                      (funcall imenu-create-index-function))))
            (check-alist imenu--index-alist)))))))

(cl-defun akirak-import--insert-line (content &key regexp inside-tree-sitter-node)
  ;; Save the position so the user can return the position being edited.
  (push-mark)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (cond
       ((and regexp
             (re-search-forward regexp nil t))
        (beginning-of-line)
        (open-line 1)
        (insert content))
       (inside-tree-sitter-node)
       (t
        (open-line 1)
        (insert content))))))

(provide 'akirak-import)
;;; akirak-import.el ends here
