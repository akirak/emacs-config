;;; akirak-treesit.el --- Treesit-based navigation mode -*- lexical-binding: t -*-

(require 'treesit)

(defvar akirak-treesit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [remap forward-word] #'akirak-treesit-forward-word)
    ;; (define-key map [remap backward-word] #'akirak-treesit-backward-word)
    ;; (define-key map [remap kill-word] #'akirak-treesit-kill-word)
    ;; (define-key map [remap down-list] #'akirak-treesit-down-list)
    (define-key map [remap backward-up-list] #'akirak-treesit-backward-up-list)
    (define-key map [remap kill-line] #'akirak-treesit-smart-kill-line)
    map))

;;;###autoload
(define-minor-mode akirak-treesit-mode
  "Navigation mode based on treesit.el."
  :lighter " TSit")

(defun akirak-treesit-forward-word (n)
  (interactive "P")
  (let ((node (treesit-node-at (point))))
    (cond
     ((or (not n)
          (and (numberp n)
               (> n 0)))
      (if (> (treesit-node-end node)
             (point))
          (progn
            (goto-char (treesit-node-end node))
            (when (looking-at (rx (+ blank)))
              (goto-char (match-end 0))))
        (while (= (treesit-node-end node) (point))
          (setq node (treesit-node-parent node)))
        (let* ((siblings (treesit-node-children node))
               (sibling (seq-find `(lambda (x)
                                     (> (treesit-node-start x) ,(point)))
                                  siblings)))
          (goto-char (treesit-node-start sibling))))
      (when (and (numberp n)
                 (> n 1))
        (akirak-treesit-forward-word (1- n))))
     ((= n 0))
     ((< n 0)
      (akirak-treesit-backward-word (- n))))))

(defun akirak-treesit-backward-word (n)
  (interactive "P")
  (let ((node (treesit-node-at (point))))
    (if (< (treesit-node-start node) (point))
        (progn
          (goto-char (treesit-node-start node))
          (when (and n (> n 1))
            (akirak-treesit-backward-word (1- n))))
      (if-let* ((parent (treesit-node-parent node))
                (children (seq-filter `(lambda (node)
                                         (< (treesit-node-start node)
                                            ,(point)))
                                      (treesit-node-children parent))))
          (progn
            (goto-char (treesit-node-start (car (last children))))
            (when (and n (> n 1))
              (akirak-treesit-backward-word (1- n))))
        (backward-word n)))))

(defun akirak-treesit-kill-word (n)
  (interactive "P")
  (let ((node (treesit-node-at (point))))
    (cond
     ((and (numberp n)
           (= n 0)))
     ((or (not n)
          (and (numberp n)
               (> n 0)))
      (delete-region (point) (treesit-node-end node))
      (when (looking-at (rx (+ blank)))
        (delete-region (point) (match-end 0)))
      (when (and (numberp n)
                 (> n 0))
        (akirak-treesit-kill-word (1- n)))))))

(defun akirak-treesit-down-list ()
  (interactive)
  (let ((start (point)))
    (cl-labels
        ((pred (x)
           (when-let (child (cadr (treesit-node-children x)))
             (when (> (treesit-node-start child)
                      start)
               child)))
         (go (node)
           (let ((nodes (treesit-node-children (treesit-node-parent node))))
             (if-let (child (seq-some #'pred nodes))
                 (goto-char (treesit-node-start child))
               (go (treesit-node-parent node))))))
      (go (treesit-node-at (point))))))

(defun akirak-treesit-backward-up-list ()
  (interactive)
  (let ((node (treesit-node-at (point)))
        parent)
    (while (and (setq parent (treesit-node-parent node))
                (= (point) (treesit-node-start parent)))
      (setq node parent))
    (goto-char (treesit-node-start parent))))

(defvar akirak-treesit-expand-region-node nil)

;;;###autoload
(defun akirak-treesit-expand-region ()
  (interactive)
  (unless (and akirak-treesit-expand-region-node
               (use-region-p)
               (eq (treesit-node-start akirak-treesit-expand-region-node)
                   (region-beginning))
               (eq (treesit-node-end akirak-treesit-expand-region-node)
                   (region-end)))
    (setq akirak-treesit-expand-region-node nil))
  (let* ((current-bounds (region-bounds))
         (node (if akirak-treesit-expand-region-node
                   (or (treesit-node-parent akirak-treesit-expand-region-node)
                       (user-error "Root node of the document"))
                 (treesit-node-at (point))))
         (start (treesit-node-start node)))
    (when (use-region-p)
      (deactivate-mark))
    (goto-char start)
    (let ((inhibit-message t))
      (activate-mark))
    (goto-char (treesit-node-end node))
    (push-mark)
    (goto-char start)
    (setq akirak-treesit-expand-region-node node)
    (if (equal (region-bounds) current-bounds)
        (akirak-treesit-expand-region)
      (message "%s" (treesit-node-type node)))))

(defcustom akirak-treesit-balanced-nodes
  '("jsx_opening_element"
    ;; heex
    "start_tag")
  "List of node types that needs balancing."
  :type '(repeat string))

(defcustom akirak-treesit-sexp-end-delimiters
  '("," ";" ";;")
  ""
  :type '(repeat string))

(defun akirak-treesit-smart-kill-line (&optional arg)
  (interactive "P")
  (if (or (numberp arg)
          (looking-at (rx (* blank) eol)))
      (kill-line arg)
    (let ((start (point))
          (node (if (looking-at (rx (+ blank)))
                    (treesit-node-at (match-end 0))
                  (treesit-node-at (point))))
          (bound (save-excursion
                   (re-search-forward (rx (* blank) eol))
                   (match-beginning 0)))
          parent)
      (cl-flet
          ((node-at-point-p (t)
             (and (<= (treesit-node-start t) (point))
                  (< (point) (treesit-node-end t)))))
        (if (node-at-point-p node)
            (if (< (treesit-node-start node) (point))
                (kill-region (point) (min bound (treesit-node-end node)))
              (catch 'stop
                (while (setq parent (treesit-node-parent node))
                  (when (and (or (< (treesit-node-start parent) start)
                                 (and (= (treesit-node-end node) bound)
                                      (looking-back (rx bol (* blank)) (line-beginning-position))))
                             (not (member (treesit-node-type node)
                                          akirak-treesit-balanced-nodes)))
                    (throw 'stop t))
                  (setq node parent)))
              (if parent
                  (if-let (end-node (akirak-treesit--find-last-node node parent bound))
                      (kill-region (point) (akirak-treesit--after-last-node (treesit-node-end end-node)))
                    ;; No node to delete, fallback to the default behavior
                    (kill-line))
                (kill-region (point) (akirak-treesit--after-last-node (treesit-node-end node)))))
          ;; There is no node at point, so find the next node and delete until
          ;; the start of the node
          (setq parent (treesit-node-parent node))
          (while (not (node-at-point-p parent))
            (setq parent (treesit-node-parent parent)))
          (if-let (node (seq-find `(lambda (node)
                                     (> (treesit-node-start node) ,(point)))
                                  (treesit-node-children parent)))
              (kill-region (point) (treesit-node-start node))
            (kill-region (point) (treesit-node-end parent))))))))

(defun akirak-treesit--after-last-node (pos)
  (save-excursion
    (goto-char pos)
    (if (looking-at (rx-to-string `(and (or ,@akirak-treesit-sexp-end-delimiters)
                                        (* blank))))
        (match-end 0)
      pos)))

(defun akirak-treesit--find-last-node (start-node parent bound)
  (when-let (nodes (thread-last
                     (cl-member start-node (treesit-node-children parent)
                                :test #'treesit-node-eq)
                     (seq-take-while `(lambda (x)
                                        (< (treesit-node-start x) ,bound)))))
    (car (last nodes (when (or (memq (char-after (1- (treesit-node-end parent)))
                                     (string-to-list "\"'>"))
                               (save-excursion
                                 (goto-char (treesit-node-end parent))
                                 (funcall show-paren-data-function)))
                       2)))))

;;;###autoload
(defun akirak-treesit-raise-node ()
  "Replace the parent node with the current node.

This is primarily intended for editing JSX/TSX."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (start (treesit-node-start node))
         (parent node))
    (while (= start (treesit-node-start parent))
      (setq node parent)
      (setq parent (treesit-node-parent node)))
    (let ((string (buffer-substring (treesit-node-start node)
                                    (treesit-node-end node)))
          (pos (treesit-node-start parent)))
      (delete-region pos (treesit-node-end parent))
      (goto-char pos)
      (save-excursion (insert string)))))

;;;###autoload
(defun akirak-treesit-jsx-close-tag ()
  (interactive)
  (if-let (open-tag (save-excursion
                      (catch 'jsx-open-tag
                        (let ((bound (point)))
                          (while (search-backward "<" nil t)
                            (let ((node (thread-last
                                          (treesit-node-at (point))
                                          (treesit-node-parent))))
                              (when (and (equal (treesit-node-type node)
                                                "jsx_opening_element")
                                         (> (treesit-node-end (treesit-node-parent node))
                                            bound))
                                (throw 'jsx-open-tag node))))))))
      (pcase-exhaustive (treesit-node-children open-tag)
        ((and `(,_ ,identifier ,_ . ,_)
              (guard (equal (treesit-node-type identifier)
                            "identifier")))
         (insert (format "</%s>" (treesit-node-text identifier)))))
    (error "Cannot find")))

;;;###autoload
(defun akirak-treesit-rename-tag (new-name)
  "Rename the HTML tag at point."
  (interactive "sRename tag: " tsx-ts-mode)
  (pcase-exhaustive (treesit-language-at (point))
    (`tsx
     (let ((node (treesit-node-at (point))))
       (while (and node (not (member (treesit-node-type node)
                                     '("jsx_opening_element" "jsx_closing_element"))))
         (setq node (treesit-node-parent node)))
       (let* ((parent (treesit-node-parent node))
              (children (treesit-node-children parent))
              other-node)
         (unless (equal (treesit-node-type parent) "jsx_element")
           (error "Failed to reach a JSX element. Instead it was %s"
                  (treesit-node-type parent)))
         (pcase-exhaustive (treesit-node-type node)
           ("jsx_opening_element"
            (setq other-node (car (last children)))
            (unless (equal (treesit-node-type other-node) "jsx_closing_element")
              (error "Failed to reach a closing element")))
           ("jsx_closing_element"
            (setq other-node (car children))
            (unless (equal (treesit-node-type other-node) "jsx_opening_element")
              (error "Failed to reach an opening element"))))
         (pcase-exhaustive (seq-sort-by #'treesit-node-start #'< (list node other-node))
           (`(,open-node ,close-node)
            (let ((start (treesit-node-start open-node)))
              (cl-flet
                  ((rename-tag (node)
                     (catch 'renamed
                       (dolist (child (treesit-node-children node))
                         (when (equal (treesit-node-type child) "identifier")
                           (goto-char (treesit-node-start child))
                           (delete-region (treesit-node-start child)
                                          (treesit-node-end child))
                           (insert new-name)
                           (throw 'renamed t))))))
                (rename-tag close-node)
                (goto-char start)
                (let ((fresh-start (treesit-node-at (point))))
                  (while (not (equal (treesit-node-type fresh-start)
                                     "jsx_opening_element"))
                    (setq fresh-start (treesit-node-parent fresh-start)))
                  (rename-tag fresh-start))
                (message "Renamed the tag to '%s'" new-name))))))))))

;;;; Other utilities for tree-sitter support

;;;###autoload
(defun akirak-treesit-list-grammars ()
  (interactive)
  (let ((dirs (append treesit-extra-load-path
                      ;; Where is the system default locations for dynamic
                      ;; libraries?
                      (list (expand-file-name "tree-sitter" user-emacs-directory))))
        grammar-alist)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (filename (directory-files dir))
          (when (string-match (concat "^libtree-sitter-\\([^z-a]+\\)"
                                      (regexp-opt-group dynamic-library-suffixes)
                                      "\\'")
                              filename)
            (cl-pushnew (cons (match-string 1 filename)
                              (file-truename (expand-file-name filename dir)))
                        grammar-alist)))))
    (cl-labels
        ((annotator (candidate)
           (concat " " (cdr (assoc candidate grammar-alist))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata (list (cons 'category 'tree-sitter-grammar)
                                     (cons 'annotation-function #'annotator)))
             (complete-with-action action grammar-alist string pred))))
      (completing-read "Tree-sitter grammar: " #'completions nil t))))

(provide 'akirak-treesit)
;;; akirak-treesit.el ends here
