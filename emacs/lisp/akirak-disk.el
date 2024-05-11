;;; akirak-disk.el ---  -*- lexical-binding: t -*-

(defconst akirak-disk-ignore-regexp
  (rx bol "/"
      ;; /run can contain systemd-mounted mount points, so don't exclude them
      ;; entirely.
      (or "sys"
          "proc"
          "dev")
      (or eol "/")))

;;;###autoload
(defun akirak-disk-findmnt ()
  "Visit a mountpoint"
  (interactive)
  (find-file (akirak-disk-read-findmnt "Mount point: ")))

(defun akirak-disk-read-findmnt (prompt)
  (let* ((alist (thread-last
                  ;; Don't use --real as it would exclude systemd-mounted
                  ;; devices.
                  (akirak-disk--findmnt-run "--list")
                  (assq 'filesystems)
                  (cdr)
                  (cl-remove-if (lambda (x)
                                  (or (member (cdr (assq 'fstype x))
                                              '("tmpfs"
                                                "ramfs"
                                                "overlay"))
                                      (string-match-p akirak-disk-ignore-regexp
                                                      (cdr (assq 'target x))))))
                  (mapcar (lambda (x)
                            (cons (file-name-as-directory
                                   (cdr (assq 'target x)))
                                  x)))))
         (candidates (mapcar #'car alist)))
    (cl-labels
        ((annotator (candidate)
           (thread-last
             (assoc candidate alist)
             (cdr)
             (assq 'source)
             (cdr)
             (concat " ")))
         (group (candidate transform)
           (if transform
               candidate
             (thread-last
               (assoc candidate alist)
               (cdr)
               (assq 'fstype)
               (cdr))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'mountpoint)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action candidates string pred))))
      (completing-read prompt #'completions
                       nil t))))

(defun akirak-disk--findmnt-run (&rest args)
  (with-temp-buffer
    (apply #'call-process
           "findmnt" nil (list t nil) nil
           "--json"
           args)
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :array-type 'list)))

;;;###autoload
(defun akirak-disk-insert-blkid ()
  (interactive)
  (let* ((props (akirak-disk--complete-blkid "Select a device: "))
         (device (cdr (assq 'DEVNAME props)))
         (candidates (thread-last
                       props
                       (mapcar (pcase-lambda (`(,prop . ,value))
                                 (pcase prop
                                   (`DEVNAME
                                    value)
                                   (`PARTUUID
                                    (concat "/dev/disk/by-partuuid/" value))
                                   (`UUID
                                    (concat "/dev/disk/by-uuid/" value))
                                   (`LABEL
                                    (concat "/dev/disk/by-label/" value)))))
                       (delq nil))))
    (insert (completing-read (format-prompt "Insert a device path" device)
                             candidates
                             nil nil nil nil device))))

(defun akirak-disk--complete-blkid (prompt)
  (let* ((fdisk (akirak-disk--parse-fdisk-list))
         (fdisk-disks (cdr (assq 'disks fdisk)))
         (fdisk-devices (cdr (assq 'devices fdisk)))
         (alist (mapcar (lambda (props)
                          (cons (alist-get 'DEVNAME props)
                                props))
                        (akirak-disk--parse-blkid))))
    (cl-labels
        ((annotator (candidate)
           (let ((props (cdr (assoc candidate alist))))
             (concat " "
                     (or (cdr (assoc candidate fdisk-devices))
                         (cdr (assoc "Size" (cdr (assoc candidate fdisk-disks)))))
                     " "
                     (or (cdr (assq 'TYPE props))
                         (cdr (assq 'PARTLABEL props)))
                     (when-let (label (cdr (assq 'LABEL props)))
                       (format " \"%s\"" label)))))
         (describe-disk (device)
           (let* ((info (cdr (assoc device fdisk-disks)))
                  (size (cdr (assoc "Size" info)))
                  (model (cdr (assoc "Disk model" info))))
             (when info
               (format " (%s%s)"
                       size
                       (if model
                           (concat " " (string-trim-right model))
                         "")))))
         (group (candidate transform)
           (if transform
               candidate
             (when-let (group (save-match-data
                                (pcase candidate
                                  ((rx bol "/dev/mapper/")
                                   "dm")
                                  ((rx bol (group "/dev/nvme" (+ anything)) "p" (+ digit) eol)
                                   (match-string 1 candidate))
                                  ((rx bol (group "/dev/" (+ anything)) (+ digit) eol)
                                   (match-string 1 candidate)))))
               (concat group (describe-disk group)))))
         (completions (string pred action)
           (if (eq action 'metadata)
               (cons 'metadata
                     (list (cons 'category 'category)
                           (cons 'group-function #'group)
                           (cons 'annotation-function #'annotator)))
             (complete-with-action action alist string pred))))
      (cdr (assoc (completing-read prompt #'completions)
                  alist)))))

(defun akirak-disk--parse-blkid ()
  (let (group
        groups)
    (cl-flet
        ((add-group ()
           (push group groups)
           (setq group nil)))
      (dolist (s (process-lines "blkid" "-o" "export" "-d"))
        (if (string-empty-p s)
            (add-group)
          (when (string-match (rx bol (group (+ (any "_" alpha)))
                                  "="
                                  (group (+ anything)))
                              s)
            (push (cons (intern (match-string 1 s))
                        (match-string 2 s))
                  group))))
      (add-group))
    groups))

(defun akirak-disk--parse-fdisk-list ()
  (let (disks
        devices)
    (with-temp-buffer
      (call-process "fdisk" nil (list t nil) nil "-l")
      ;; Collect disk information
      (goto-char (point-min))
      (let (device size info)
        (while (re-search-forward (rx bol "Disk " (group (+? anything)) ": "
                                      (group (+ digit) "." (+ digit)
                                             " " (+ alpha) "B"))
                                  nil t)
          (setq device (match-string 1)
                size (match-string 2))
          (forward-line 1)
          (while (looking-at (rx bol (group (+? nonl)) ": " (group (+ nonl))))
            (push (cons (match-string 1)
                        (match-string 2))
                  info)
            (forward-line 1))
          (push (cons device
                      (cons (cons "Size" size)
                            info))
                disks)))
      ;; Collect partition information
      (goto-char (point-min))
      (while (re-search-forward (rx bol "/dev/" (+ (not (any blank)))) nil t)
        (push (cons (match-string 0)
                    (buffer-substring-no-properties (point) (line-end-position)))
              devices)))
    `((disks . ,disks)
      (devices . ,devices))))

(provide 'akirak-disk)
;;; akirak-disk.el ends here
