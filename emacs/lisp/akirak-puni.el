;;; akirak-puni.el ---  -*- lexical-binding: t -*-

(require 'puni)

;;;; Generic setup command

(defcustom akirak-puni-setup-alist
  '(((heex-ts-mode)
     akirak-puni-heex-ts-setup)
    ((elixir-ts-mode)
     akirak-puni-elixir-ts-setup)
    ((tsx-mode js-jsx-mode tsx-ts-mode typescript-ts-mode)
     akirak-puni-jsx-setup)
    ((svelte-mode)
     akirak-puni-svelte-setup)
    ((elixir-mode)
     akirak-puni-elixir-setup))
  ""
  :type '(alist :key-type (repeat symbol)
                :value-type (cons function (cons nil))))

;;;###autoload
(defun akirak-puni-mode-setup ()
  (when-let* ((mode (apply #'derived-mode-p
                           (apply #'append (mapcar #'car akirak-puni-setup-alist))))
              (ent (seq-find `(lambda (cell) (memq ',mode (car cell)))
                             akirak-puni-setup-alist))
              (setup-fn (cadr ent)))
    (message "Detected %s: Running %s" mode setup-fn)
    (funcall setup-fn)))

;;;; Mode-specific

;;;;; JSX

;;;###autoload
(defun akirak-puni-jsx-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (if (require 'tagedit nil t)
      (local-set-key [remap puni-kill-line] #'akirak-puni-jsx-kill-line)
    (message "Warning[akirak-puni]: JSX is detected, but tagedit is unavailable.")))

;;;###autoload
(defun akirak-puni-jsx-kill-line ()
  (interactive)
  (cond
   ((looking-at (rx (* blank) (group "</")))
    (goto-char (match-beginning 1))
    (push-mark)
    (search-forward ">")
    (delete-region (mark) (point)))
   ((looking-at (rx (* blank) "<"))
    (tagedit-kill))
   (t
    (puni-soft-delete-by-move #'akirak-puni-jsx-end-of-soft-kill
                              nil
                              'beyond
                              'kill
                              'delete-one))))

(defun akirak-puni-jsx-end-of-soft-kill ()
  (cond
   ((eolp)
    (forward-char))
   ;; Kill content inside a tag (i.e. between "<" and ">")
   ((and (looking-back (rx "<" (* (not (any ">"))))
                       (line-beginning-position)))
    (if (re-search-forward (rx (? "/") ">") (line-end-position) t)
        (goto-char (match-beginning 0))
      (end-of-line)))
   ;; Kill content inside a tag pair (i.e. between an open tag and end tag)
   ((looking-back (rx ">" (* (not (any "<"))))
                  (line-beginning-position))
    (if (re-search-forward "<" (line-end-position) t)
        (goto-char (match-beginning 0))
      (end-of-line)))
   (t
    (end-of-line))))

;;;;; Svelte

;;;###autoload
(defun akirak-puni-svelte-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (if (require 'tagedit nil t)
      (local-set-key [remap puni-kill-line] #'akirak-puni-svelte-kill-line)
    (message "Warning[akirak-puni]: Svelte is detected, but tagedit is unavailable.")))

(defalias 'akirak-puni-svelte-kill-line #'akirak-puni-jsx-kill-line)

;;;;; Elixir

;;;###autoload
(defun akirak-puni-heex-ts-setup ()
  "Setup puni bindings for heex."
  (interactive)
  (if (require 'tagedit nil t)
      (local-set-key [remap puni-kill-line] #'akirak-puni-heex-ts-kill-line)
    (message "Warning[akirak-puni]: Heex is detected, but tagedit is unavailable.")))

(defun akirak-puni-heex-ts-kill-line ()
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (parent (treesit-node-parent node)))
    (if (string-prefix-p "<%" (treesit-node-text parent))
        (let ((start (treesit-node-start node)))
          (save-excursion
            (if (search-forward "\n" (treesit-node-end parent) t)
                (delete-region start (point))
              (delete-region start
                             (treesit-node-end (car (last (treesit-node-children parent)
                                                          2)))))))
      (akirak-puni-jsx-kill-line))))

(defun akirak-puni-elixir-ts-setup ()
  (local-set-key [remap puni-kill-line] #'akirak-puni-elixir-ts-kill-line))

;;;###autoload
(defun akirak-puni-elixir-ts-kill-line (&optional arg)
  "Kill a line forward while keeping expressions balanced."
  (interactive "P")
  (require 'pcase)
  (require 'rx)
  (cond
   ((numberp arg)
    (kill-line arg))
   ((looking-at (rx (* blank) eol))
    (if (looking-back (rx bol (* blank)) (line-beginning-position))
        (progn
          (delete-blank-lines)
          (back-to-indentation))
      (delete-region (point) (line-beginning-position 2))))
   (t
    (let* ((node (treesit-node-at (point)))
           (parent (treesit-node-parent node)))
      (pcase (treesit-node-text node)
        ((rx bos
             (or (any "~@%\"{[(")
                 "test"
                 "do")
             eos)
         (delete-region (point) (treesit-node-end parent)))
        ((rx bos "def" (* lower))
         (delete-region (point) (treesit-node-end parent)))
        ((rx bos "<" (?  "%"))
         (delete-region (point) (treesit-node-end parent)))
        (_
         (pcase (treesit-node-text (treesit-node-parent node))
           ((rx bos (any "{[(\""))
            (delete-region
             (point)
             (min (line-end-position)
                  (treesit-node-end (car (last (treesit-node-children parent)
                                               2))))))
           ((rx bos "<%")
            (let ((start (treesit-node-start node)))
              (save-excursion
                (if (search-forward "\n" (treesit-node-end parent) t)
                    (delete-region start (point))
                  (delete-region start (treesit-node-end (car (last (treesit-node-children parent)
                                                                    2))))))))
           (_
            (if (< (treesit-node-end parent) (line-end-position))
                (delete-region (point) (treesit-node-end parent))
              (kill-line arg))))))
      (when (looking-at (rx "," (* blank) eol))
        (delete-region (point) (match-end 0)))))))

(defvar akirak-puni-elixir-opener-regexp nil)

(defun akirak-puni-elixir-setup ()
  (require 'elixir-smie)
  (local-set-key [remap puni-kill-line] #'akirak-puni-elixir-kill-line))

;;;###autoload
(defun akirak-puni-elixir-kill-line ()
  "Kill a line forward while keeping expressions balanced."
  (interactive)
  (puni-soft-delete-by-move #'akirak-puni-elixir-end-of-soft-kill
                            nil
                            'beyond
                            ;; 'within
                            'kill
                            'delete-one))

(defun akirak-puni-elixir-end-of-soft-kill ()
  (unless akirak-puni-elixir-opener-regexp
    (setq-local akirak-puni-elixir-opener-regexp
                (rx-to-string
                 `(or ,@(seq-uniq (mapcar #'car smie-closer-alist))))))
  (when (looking-at (rx (+ blank)))
    (re-search-forward (rx (+ blank))))
  (cond
   ((eolp)
    (forward-char))
   ((looking-at (rx symbol-start "def"))
    (end-of-defun))
   ((looking-at (rx symbol-start "do" symbol-end))
    (ruby-end-of-block))
   ((looking-at (rx "#"))
    (end-of-line))
   ((looking-at (rx (?  "@" (or "doc" "moduledoc") (+ space))
                    "\"\"\""))
    (goto-char (nth 1 (match-data)))
    (re-search-forward (rx "\"\"\""
                           (* "\n"))))
   ((looking-at akirak-puni-elixir-opener-regexp)
    (smie-forward-sexp))
   ((looking-back (rx (syntax open-parenthesis)) 1)
    (goto-char (match-beginning 0))
    (forward-sexp)
    (backward-char))
   ((and (looking-at (rx symbol-start))
         (looking-back (rx symbol-start "def" (* alpha) (+ space))
                       (line-beginning-position)))
    (if (re-search-forward (rx (* space) (or "," "do")) (line-end-position) t)
        (goto-char (car (match-data)))
      (end-of-line)))
   (t
    (end-of-line))))

(provide 'akirak-puni)
;;; akirak-puni.el ends here
