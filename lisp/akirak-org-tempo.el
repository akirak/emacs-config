;;; akirak-org-tempo.el --- Define tempo templates within Org -*- lexical-binding: t -*-

(require 'org)
(require 'tempo)

(defvar akirak-org-tempo-file-alist nil)

;;;###autoload
(defun akirak-org-tempo-use-file (file)
  "Add an Org FILE to the template sources of the current buffer."
  (let ((tag-list (akirak-org-tempo--tag-list file)))
    (cl-pushnew (cons tag-list file) akirak-org-tempo-file-alist)
    (tempo-use-tag-list tag-list)))

(defun akirak-org-tempo-ensure-loaded ()
  "Ensure the templates sources of the current buffer are loaded."
  (interactive)
  (pcase-dolist (`(,tag-list . ,_) tempo-local-tags)
    (unless (boundp tag-list)
      (when-let* ((file (alist-get tag-list akirak-org-tempo-file-alist)))
        (akirak-org-tempo-load file)))))

(defun akirak-org-tempo-load (file)
  "Load templates from an Org FILE."
  (interactive (list (buffer-file-name))
               org-mode)
  (unless file
    (user-error "Load from an Org file"))
  (let ((tag-list (akirak-org-tempo--tag-list file))
        (prefix (concat (file-name-base file) "-"))
        (re-property (org-re-property "tempo_tag"))
        (count 0))
    (set tag-list nil)
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward re-property nil t)
         (let ((tag (match-string 3))
               (bound (org-entry-end-position)))
           (catch 'found-block
             (while (re-search-forward org-babel-src-block-regexp bound t)
               ;; If the source language is tempo, it is a block.
               (let ((language (match-string 2)))
                 (when (member language '("tempo" "emacs-lisp" "elisp"))
                   (condition-case-unless-debug nil
                       (let* ((pos (match-beginning 0))
                              (element (org-element-at-point pos))
                              (raw-parameters (org-element-property :parameters element))
                              (params (org-babel-parse-header-arguments raw-parameters 'no-eval))
                              (src (org-element-property :value element))
                              (name (concat prefix tag))
                              (func (intern (concat "tempo-template-" name))))
                         (pcase language
                           ("tempo"
                            (tempo-define-template
                             name
                             (read src)
                             tag
                             ;; Use the comment at the first line, if any, as the
                             ;; template description.
                             (if (string-match (rx bol (* blank) (+ ";")
                                                   (* blank) (group (+ nonl)))
                                               src)
                                 (match-string 1 src)
                               (org-entry-get pos "ITEM"))
                             tag-list))
                           ((or "emacs-lisp" "elisp")
                            (let ((template-name (eval (read src))))
                              (setq func template-name)
                              (tempo-add-tag tag func tag-list))))
                         (akirak-org-tempo--advice-add func params)
                         (cl-incf count))
                     (error (message "Error while loading tempo buffer")))
                   (throw 'found-block t))))
             (message "Not found a tempo block even in the presence of tempo_tag property at %d in %s"
                      (point)
                      (buffer-file-name)))))
       (message "Loaded %d templates from %s" count (file-name-nondirectory file))))))

(defun akirak-org-tempo--advice-add (target-symbol alist)
  (when alist
    (let ((symbol (intern (format "ad-around-%s" target-symbol)))
          (no-save-mark (alist-get :no-save-mark alist))
          (pre (alist-get :pre alist))
          (post (alist-get :post alist)))
      (fset symbol
            `(lambda (orig &rest args)
               ;; Push the marker so the user can return to the original position after
               ;; expansion.
               (unless ,no-save-mark
                 (push-mark))
               (atomic-change-group
                 ,(when pre
                    (read pre))
                 (apply orig args)
                 ,(when post
                    (read post)))))
      (advice-add target-symbol :around symbol))))

(defun akirak-org-tempo--tag-list (file)
  (intern (concat (file-name-base file) "-tempo-tags")))

(provide 'akirak-org-tempo)
;;; akirak-org-tempo.el ends here
