;;; akirak-dashboard.el --- Custom building blocks for dashboard -*- lexical-binding: t -*-

;;;; org-clock-history

(defun akirak-dashboard-org-clock-items (list-size)
  ;; (require 'akirak-org-clock)
  (let (els)
    (catch 'dashboard-org-clock-finish
      (dolist (marker org-clock-history)
        (when (= list-size (length els))
          (throw 'dashboard-org-clock-finish t))
        (catch 'dashboard-org-clock-skip
          (when (and (markerp marker)
                     (buffer-live-p (marker-buffer marker)))
            (org-with-point-at marker
              (org-back-to-heading)
              (let ((el (org-element-at-point-no-context)))
                ;; Exclude items that I won't work on soon. If I need to work on
                ;; an item, it should have an active and unfinished todo keyword
                ;; and it should not have a future scheduled timestamp.
                (when (or (memq (org-element-property :todo-type el)
                                '(nil done))
                          (member (org-element-property :todo-keyword el)
                                  '("CASUAL"
                                    "STOPPED"))
                          (org-element-property :archivedp el)
                          (when-let (ts (org-element-property :scheduled el))
                            (time-less-p (current-time) (org-timestamp-to-time ts))))
                  (throw 'dashboard-org-clock-skip t))
                (push (thread-first
                        el
                        (org-element-put-property :CATEGORY (org-entry-get nil "CATEGORY" t))
                        (org-element-put-property :hd-marker (point-marker)))
                      els)))))))
    (nreverse els)))

(defun akirak-dashboard-org-clock-format-item (el)
  (concat (if-let (kwd (org-element-property :todo-keyword el))
              (concat (org-no-properties kwd) " ")
            "")
          (format "[%s] "
                  (or (org-element-property :CATEGORY el)
                      "?"))
          (org-link-display-format (org-element-property :raw-value el))
          (if-let (ts (org-element-property :deadline el))
              (concat " D" (org-element-property :raw-value ts))
            "")))

(provide 'akirak-dashboard)
;;; akirak-dashboard.el ends here
