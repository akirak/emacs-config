;;; akirak-xdg.el --- XDG integration -*- lexical-binding: t -*-

(require 'xdg)
(require 'map)
(require 'seq)

(defcustom akirak-xdg-desktop-command-prefix "akirak/run-"
  ""
  :type 'string)

(defcustom akirak-xdg-desktop-whitelist nil
  ""
  :type '(repeat string))

(defvar akirak-xdg-desktop-cache nil)

;;;###autoload
(defun akirak-xdg-load-desktop-apps (&optional arg)
  "Load the desktop applications for the environment."
  (interactive "P")
  (akirak-xdg--load-desktop-files)
  (if arg
      (dolist (desktop (akirak-xdg-complete-desktop-file "Select applications: "
                                                         'multi))
        (akirak-xdg-define-desktop-command
         desktop (map-elt akirak-xdg-desktop-cache desktop)))
    (map-do (lambda (desktop data)
              (let* ((command (map-elt data "Exec"))
                     (prog (if (string-match (rx bol (* blank)
                                                 "'" (group (+? anything)) "'")
                                             command)
                               (match-string 1 command)
                             (car (split-string command))))
                     (progname (file-name-nondirectory prog)))
                (when (seq-some `(lambda (regexp)
                                   (string-match-p regexp ,progname))
                                akirak-xdg-desktop-whitelist)
                  (akirak-xdg-define-desktop-command desktop data))))
            akirak-xdg-desktop-cache)))

(defun akirak-xdg-define-desktop-command (desktop data)
  (let* ((name (akirak-xdg--escape-name (file-name-base desktop)))
         (symbol (intern (concat akirak-xdg-desktop-command-prefix name)))
         (documentation
          (format-spec "Run %n%g%s."
                       `((?n . ,(map-elt data "Name"))
                         (?g . ,(if (map-elt data "GenericName")
                                    (format " (%s)"
                                            (map-elt data "GenericName"))
                                  ""))
                         (?s . ,(if (equal (map-elt data "Terminal")
                                           "true")
                                    " in terminal"
                                  ""))))))
    (fset symbol
          `(lambda (&optional arg)
             ,documentation
             (interactive "P")
             (akirak-xdg--run-desktop-entry
              ,desktop
              ',(map-into data 'alist)
              (akirak-xdg--maybe-file-argument arg))))
    (message "Defined a command: %s" symbol)))

;;;###autoload
(defun akirak-xdg-run-desktop-app (&optional arg)
  (interactive "P")
  (unless akirak-xdg-desktop-cache
    (akirak-xdg--load-desktop-files))
  (let* ((filename (akirak-xdg-complete-desktop-file "Run desktop app: "))
         (data (gethash filename akirak-xdg-desktop-cache)))
    (akirak-xdg--run-desktop-entry filename data
                                   (akirak-xdg--maybe-file-argument arg))))

(defun akirak-xdg--maybe-file-argument (arg)
  (pcase arg
    (`nil)
    ((pred stringp)
     arg)
    ((guard (derived-mode-p 'dired-mode))
     (dired-file-name-at-point))
    (_
     (read-file-name "Run command on: "
                     nil nil 'mustmatch))))

(defun akirak-xdg--run-desktop-entry (desktop data &optional file)
  (akirak-xdg--run-command (map-elt data "Exec") file
                           :name (file-name-base desktop)
                           :terminal (equal (string-trim (map-elt data "Terminal"))
                                            "true")))

(cl-defun akirak-xdg--run-command (command &optional arg &key name terminal)
  (cl-assert (or (null arg)
                 (and (string-match-p (rx "%U") command)
                      (file-exists-p arg))))
  (when terminal
    (require 'akirak-shell))
  (let* ((command (string-replace "%U" (if arg
                                           (shell-quote-argument arg)
                                         "")
                                  command))
         (name (or name
                   (format "%s %s"
                           (if terminal
                               "term-exec"
                             "exec")
                           command)))
         (buffer-name (format "*%s*" name)))
    (if-let* ((buffer (get-buffer buffer-name))
              (process (get-buffer-process buffer))
              (live (process-live-p process)))
        (if terminal
            (akirak-xdg--switch-to-buffer buffer)
          (user-error "%s is already running on the desktop" (or name command)))
      (if terminal
          (akirak-shell-eat-new :dir "~/"
                                :name name
                                :window 'new-tab
                                :command (list "sh" "-c" command))
        (start-process (or name "xdg-command")
                       (generate-new-buffer buffer-name)
                       "sh" "-c" command)))))

(defun akirak-xdg--switch-to-buffer (buffer)
  (if-let* ((window (get-buffer-window buffer)))
      (select-window window)
    (if-let* ((tab (tab-bar-get-buffer-tab buffer)))
        (tab-bar-switch-to-tab (alist-get 'name (cdr tab)))
      (pop-to-buffer buffer))))

(defun akirak-xdg-complete-desktop-file (prompt &optional multiple)
  (let ((completion-extra-properties
         `(:annotation-function
           (lambda (candidate)
             (when-let* ((data (gethash candidate
                                        akirak-xdg-desktop-cache)))
               (format-spec " [%g] %x — %c"
                            `((?g . ,(or (gethash "GenericName" data)
                                         ""))
                              (?x . ,(gethash "Exec" data))
                              (?c . ,(or (gethash "Comment" data)
                                         "")))))))))
    (funcall (if multiple
                 #'completing-read-multiple
               #'completing-read)
             prompt (map-keys akirak-xdg-desktop-cache)
             nil t)))

(defun akirak-xdg--load-desktop-files ()
  (if akirak-xdg-desktop-cache
      (clrhash akirak-xdg-desktop-cache)
    (setq akirak-xdg-desktop-cache (make-hash-table :size 500 :test #'equal)))
  (dolist (file (akirak-xdg--desktop-files))
    (let ((name (file-name-nondirectory file)))
      (unless (gethash name akirak-xdg-desktop-cache)
        (let ((value (xdg-desktop-read-file file)))
          (puthash name value akirak-xdg-desktop-cache))))))

(defun akirak-xdg--desktop-files ()
  (let (files)
    (dolist (data-dir (append (ensure-list (xdg-data-home))
                              (xdg-data-dirs)))
      (let ((apps-dir (file-name-concat data-dir "applications")))
        (when (file-directory-p apps-dir)
          (setq files (append files (akirak-xdg--desktop-files-in-dir apps-dir))))))
    files))

(defun akirak-xdg--desktop-files-in-dir (dir)
  (directory-files dir 'full "\\.desktop\\'" 'nosort))

(defun akirak-xdg--escape-name (string)
  (replace-regexp-in-string (rx (+ (any blank "._"))) "-" string))

(provide 'akirak-xdg)
;;; akirak-xdg.el ends here
