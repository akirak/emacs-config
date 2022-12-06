;;; akirak-puni.el ---  -*- lexical-binding: t -*-

(require 'puni)

;;;; Generic setup command

;;;###autoload
(defun akirak-puni-mode-setup ()
  (cl-case (derived-mode-p 'elixir-mode
                           'typescript-tsx-mode
                           'js-jsx-mode
                           'svelte-mode)
    (elixir-mode
     (akirak-puni-elixir-setup))
    ((typescript-tsx-mode js-jsx-mode)
     (if (require 'tagedit nil t)
         (akirak-puni-jsx-setup)
       (message "Warning[akirak-puni]: JSX is detected, but tagedit is unavailable.")))
    (svelte-mode
     (if (require 'tagedit nil t)
         (akirak-puni-svelte-setup)
       (message "Warning[akirak-puni]: JSX is detected, but tagedit is unavailable.")))))

;;;; Mode-specific

;;;;; JSX

;;;###autoload
(defun akirak-puni-jsx-setup ()
  "Setup puni bindings for jsx."
  (interactive)
  (local-set-key [remap puni-kill-line] #'akirak-puni-jsx-kill-line))

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
  (local-set-key [remap puni-kill-line] #'akirak-puni-svelte-kill-line))

(defalias 'akirak-puni-svelte-kill-line #'akirak-puni-jsx-kill-line)

;;;;; Elixir

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
