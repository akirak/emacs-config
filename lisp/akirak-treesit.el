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
    (define-key map [remap open-line] #'akirak-treesit-open-line)
    (define-key map (kbd "C-M-n") #'akirak-treesit-forward-up-list)
    (define-key map (kbd "M-n") #'akirak-treesit-next-same-type-sibling)
    (define-key map (kbd "M-p") #'akirak-treesit-previous-same-type-sibling)
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
           (when-let* ((child (cadr (treesit-node-children x))))
             (when (> (treesit-node-start child)
                      start)
               child)))
         (go (node)
           (let ((nodes (treesit-node-children (treesit-node-parent node))))
             (if-let* ((child (seq-some #'pred nodes)))
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

(defun akirak-treesit-forward-up-list ()
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (start (treesit-node-start node))
         (end (treesit-node-end node))
         parent)
    (while (and (setq parent (treesit-node-parent node))
                (or (= start (treesit-node-start parent))
                    (= end (treesit-node-end parent))))
      (setq node parent))
    (goto-char (treesit-node-end parent))))

(defun akirak-treesit-next-same-type-sibling (&optional opposite)
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (start (treesit-node-start node))
         parent)
    (catch 'jumped
      (while (setq parent (treesit-node-parent node))
        (pcase (cl-member-if `(lambda (x)
                                (= ,start (treesit-node-start x)))
                             (if opposite
                                 (reverse (treesit-node-children parent))
                               (treesit-node-children parent)))
          ((and `(,self . ,siblings)
                (guard siblings))
           (when-let* ((dest (seq-find `(lambda (x)
                                          (equal (treesit-node-type x)
                                                 ,(treesit-node-type self)))
                                       siblings)))
             (goto-char (treesit-node-start dest))
             (throw 'jumped t))))
        (setq node parent)))))

(defun akirak-treesit-previous-same-type-sibling ()
  (interactive)
  (akirak-treesit-next-same-type-sibling t))

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
    "{"
    "("
    "["
    ;; heex
    "start_tag"
    "start_component")
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
      (if (looking-back (rx bol (+ blank)) (line-beginning-position))
          (let ((indentation (current-indentation)))
            (beginning-of-line 1)
            (kill-line arg)
            (when (looking-at (rx-to-string `(** 0 ,indentation blank)))
              (goto-char (match-end 0))))
        (kill-line arg))
    (or (akirak-treesit--maybe-kill-inside-string)
        (let* ((start (point))
               ;; Determine the position where the killed node(s) resides.
               (node-start (if (looking-at (rx (+ blank)))
                               (match-end 0)
                             (point)))
               (node (treesit-node-at node-start))
               (bound (save-excursion
                        (re-search-forward (rx (* blank) eol))
                        (match-beginning 0)))
               parent)
          (cl-flet
              ((node-at-point-p (t)
                 (and (<= (treesit-node-start t) node-start)
                      (< node-start (treesit-node-end t)))))
            (if (node-at-point-p node)
                (if (< (treesit-node-start node) (point))
                    (akirak-treesit--kill-line-region start (min bound (treesit-node-end node))
                                                      arg)
                  (catch 'stop
                    (while (setq parent (treesit-node-parent node))
                      (when (and (or (< (treesit-node-start parent) start)
                                     (and (= (treesit-node-end node) bound)
                                          (looking-back (rx bol (* blank)) (line-beginning-position)))
                                     (> (cdr (posn-col-row (posn-at-point (treesit-node-end node))))
                                        (cdr (posn-col-row (posn-at-point node-start)))))
                                 (not (member (treesit-node-type node)
                                              akirak-treesit-balanced-nodes)))
                        (throw 'stop t))
                      (setq node parent)))
                  (if parent
                      (if-let* ((end-node (akirak-treesit--find-last-node node parent bound)))
                          (akirak-treesit--kill-line-region
                           (point) (akirak-treesit--after-last-node (treesit-node-end end-node))
                           arg)
                        ;; No node to delete, fallback to the default behavior
                        (kill-line))
                    (akirak-treesit--kill-line-region
                     (point) (akirak-treesit--after-last-node (treesit-node-end node))
                     arg)))
              ;; There is no node at point, so find the next node and delete until
              ;; the start of the node
              (setq parent (treesit-node-parent node))
              (while (not (node-at-point-p parent))
                (setq parent (treesit-node-parent parent)))
              (if-let* ((node (seq-find `(lambda (node)
                                           (> (treesit-node-start node) ,(point)))
                                        (treesit-node-children parent))))
                  (akirak-treesit--kill-line-region (point) (treesit-node-start node)
                                                    arg)
                (akirak-treesit--kill-line-region (point) (treesit-node-end parent)
                                                  arg))))))))

(defun akirak-treesit--kill-line-region (start end &optional arg)
  (pcase-let*
      ((`(,start-at-bol-or-indent . ,end-at-bol)
        (save-excursion
          (goto-char start)
          (cons (akirak-treesit--at-bol-or-indent)
                (save-excursion
                  (goto-char end)
                  (bolp))))))
    (cond
     ((and start-at-bol-or-indent end-at-bol)
      (if arg
          (progn
            (kill-region (save-excursion
                           (goto-char start)
                           (line-beginning-position))
                         end)
            (back-to-indentation))
        (let ((indentation (current-indentation)))
          (kill-region (save-excursion
                         (goto-char start)
                         (line-beginning-position))
                       (1- end))
          (indent-to indentation))))
     (end-at-bol
      (kill-region start (1- end)))
     (t
      (kill-region start end)))))

(defun akirak-treesit--maybe-kill-inside-string ()
  (pcase (treesit-language-at (point))
    (`tsx
     (let ((node (treesit-node-at (point))))
       (when (equal (treesit-node-type node) "string_fragment")
         (delete-region (point) (min (1- (treesit-node-end node))
                                     (line-end-position))))))
    (_
     (when-let* ((string-start (ppss-comment-or-string-start (syntax-ppss))))
       (akirak-treesit--kill-line-inside-string string-start)))))

(defun akirak-treesit--kill-line-inside-string (string-start)
  (let ((pos (point))
        (line-end-pos (line-end-position))
        (bound (save-excursion
                 (goto-char string-start)
                 (pcase-exhaustive (funcall show-paren-data-function)
                   (`(,_ ,_ ,bound . ,_)
                    bound)
                   (`nil
                    (cond
                     ((and block-comment-start
                           (looking-at (regexp-quote block-comment-start)))
                      (and (search-forward block-comment-end)
                           (match-beginning 0)))
                     ((and comment-start
                           (if comment-start-skip
                               (looking-at comment-start-skip)
                             (looking-at (regexp-quote comment-start))))
                      (goto-char (match-end 0))
                      (cond
                       (comment-end-skip
                        (re-search-forward comment-end-skip)
                        (match-beginning 0))
                       (comment-end
                        (search-forward comment-end)
                        (match-beginning 0))))
                     (t
                      (let* ((open-char (char-after (point)))
                             (close-char (or (and (boundp electric-pair-mode)
                                                  (nth 2 (electric-pair-syntax-info open-char)))
                                             (matching-paren open-char)
                                             open-char)))
                        (forward-char 1)
                        (search-forward (char-to-string close-char))
                        (match-beginning 0)))))))))
    ;; To handle string interpolation, don't exceed the close bound of the
    ;; current tree-sitter node.
    (unless (< (treesit-node-end (treesit-node-at pos)) bound)
      (kill-region pos (if bound
                           (min line-end-pos bound)
                         line-end-pos))
      t)))

(defun akirak-treesit--at-bol-or-indent ()
  (looking-back (rx bol (* blank)) (line-beginning-position)))

(defun akirak-treesit--after-last-node (pos)
  (save-excursion
    (goto-char pos)
    (when (looking-at (rx-to-string `(and (or ,@akirak-treesit-sexp-end-delimiters)
                                          (* blank))))
      (goto-char (match-end 0)))
    (if (looking-at (rx (* blank) eol))
        (line-beginning-position 2)
      (point))))

(defun akirak-treesit--find-last-node (start-node parent bound)
  (when-let* ((nodes (thread-last
                       (cl-member start-node (treesit-node-children parent)
                                  :test #'treesit-node-eq)
                       (seq-take-while `(lambda (x)
                                          (< (treesit-node-start x) ,bound))))))
    (car (last nodes (when (or (memq (char-after (1- (treesit-node-end parent)))
                                     (string-to-list "\"'>"))
                               (save-excursion
                                 (goto-char (treesit-node-end parent))
                                 (funcall show-paren-data-function)))
                       2)))))

;;;###autoload
(defun akirak-treesit-open-line (&optional n)
  (interactive "p")
  (if (akirak-treesit--at-bol-or-indent)
      ;; treesit-node-at can fail, so it is necessary to add a fallback.
      (if-let* ((next-node (ignore-errors
                             (treesit-node-at (if (looking-at (rx (* blank)))
                                                  (match-end 0)

                                                (point))))))
          (let ((indentation (current-indentation))
                (on-end (and (null (treesit-node-next-sibling next-node))
                             (equal (treesit-node-type next-node)
                                    (treesit-node-text next-node)))))
            (beginning-of-line)
            (open-line n)
            (indent-to-column (if on-end
                                  (+ indentation tab-width)
                                indentation)))
        (message "Fallback after a treesit failure")
        (open-line n))
    (open-line n)))

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
  (if-let* ((open-tag (save-excursion
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
                                  (throw 'jsx-open-tag node)))))))))
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

;;;; Extras for treesit-fold

;;;###autoload
(defun akirak-treesit-fold-dwim (&optional arg)
  (interactive "P")
  (pcase arg
    (`(16)
     (treesit-fold-open-all))
    (0
     (treesit-fold-close-all))
    (1
     (when-let* ((ov (seq-find (lambda (ov)
                                 (eq 'treesit-fold (overlay-get ov 'invisible)))
                               (overlays-in (point) (point-max)))))
       (goto-char (overlay-start ov))
       (treesit-fold-open)))
    (`(4)
     (treesit-fold-open-recursively))
    (_
     (treesit-fold-toggle))))

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
