;;; akirak-consult-org.el --- Extensions to consult-org -*- lexical-binding: t -*-

(require 'consult-org)
(require 'akirak-org-clock)

;;;###autoload
(defun akirak-consult-org-clock (&optional arg)
  (interactive "P")
  (pcase arg
    (`nil (if (org-clocking-p)
              (org-clock-goto)
            (akirak-consult-org-clock-history)))
    ('(4) (akirak-consult-org-clock-history))
    ('(16) (akirak-consult-org-clock-history t))))

(defun akirak-consult-org-clock-history (&optional rebuild)
  "Clock in to an entry in the clock history."
  ;; This will trigger loading of a desired set of Org files through
  ;; `eval-after-load'.
  (interactive)
  (require 'org-agenda)
  (when (or rebuild (not org-clock-history))
    (akirak-org-clock-rebuild-history))
  (let ((selected (save-window-excursion
                    (consult--read
                     (consult--with-increased-gc (consult-org-clock--headings))
                     :prompt "Go to heading: "
                     :category 'akirak-consult-org-olp-with-file
                     :sort nil
                     :require-match t
                     :history '(:input consult-org--history)
                     :narrow (consult-org--narrow)
                     :state (consult--jump-state)
                     :lookup #'consult--lookup-candidate))))
    (org-clock-clock-in (list selected))))

;;;###autoload
(defun akirak-consult-org-olp-to-marker (_type olp)
  "Return a marker to an OLP."
  (cons 'org-marker
        (save-match-data
          (when (string-match (regexp-quote ".org/") olp)
            (org-find-olp (cons (substring-no-properties
                                 olp 0 (1- (match-end 0)))
                                (thread-first
                                  (substring-no-properties
                                   olp (match-end 0)
                                   (1- (length olp)))
                                  (string-trim-right)
                                  (split-string "/"))))))))

(defun consult-org-clock--headings ()
  ;; Based on `consult-org--headings'.
  (let (buffer)
    (cl-flet*
        ((live-p (marker)
           (and (markerp marker)
                (buffer-live-p (marker-buffer marker))))
         (abbr-file-name-1 (filename)
           (if (string-prefix-p "/" filename)
               (abbreviate-file-name filename)
             filename))
         (abbr-buffer-file-name (buffer)
           (thread-last
             (org-base-buffer buffer)
             (buffer-file-name)
             (substring-no-properties)
             (abbr-file-name-1)))
         (format-candidate (marker)
           (unless (eq buffer (marker-buffer marker))
             (setq buffer (marker-buffer marker)
                   org-outline-path-cache nil))
           (with-current-buffer (marker-buffer marker)
             (org-with-wide-buffer
              (goto-char marker)
              (pcase-let ((`(_ ,level ,todo ,prio . _) (org-heading-components))
                          (cand (org-format-outline-path
                                 (org-get-outline-path 'with-self 'use-cache)
                                 most-positive-fixnum
                                 (abbr-buffer-file-name buffer))))
                (setq cand (concat cand (consult--tofu-encode (point))))
                (add-text-properties 0 1
                                     `(consult--candidate
                                       ,(point-marker)
                                       consult-org--heading (,level ,todo . ,prio))
                                     cand)
                cand)))))
      (thread-last
        org-clock-history
        (cl-remove-if-not #'live-p)
        (mapcar #'format-candidate)
        (delq nil)))))

(provide 'akirak-consult-org)
;;; akirak-consult-org.el ends here
