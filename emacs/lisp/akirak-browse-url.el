;;; akirak-browse-url.el --- Convenient wrappers for browse-url -*- lexical-binding: t -*-

(defcustom akirak-browse-chromium-hosts
  '("airtable.com"
    "youtube.com"
    "music.youtube.com"
    "figma.com")
  "List of hosts that should be browsed using Chromium."
  :type '(repeat string))

(defvar akirak-browser-url-orig-fn nil)

(defun akirak-browse-url-private-host-p (host)
  "Return non-nil if HOST is a private/local host."
  (or (equal host "localhost")
      (akirak-browse-url-ipv4-p host)))

(defun akirak-browse-url-ipv4-p (host)
  "Return non-nil if HOST is an IPv4 address."
  (string-match-p (rx bol (+ digit) (* "." (+ digit)) eol)
                  host))

;;;###autoload
(defun akirak-browse-url-prefer-xwidget (url &optional arg)
  "Browse URL using xwidget-webkit when suitable.."
  (if (require 'xwidget nil t)
      (unless (akirak-browse-url--non-default-p url arg)
        (akirak-browse-url--xwidget url arg))
    (akirak-browse-url-external-browser url arg)))

(defun akirak-browse-url--xwidget (url &optional arg)
  ;; This is basically what `xwidget-webkit-goto-url' does, but reuse the
  ;; existing window if it is inside the same frame..
  (if-let (session (unless arg
                     (xwidget-webkit-current-session)))
      (progn
        (xwidget-webkit-goto-uri session url)
        (let ((buffer (xwidget-buffer (xwidget-webkit-current-session))))
          (if-let (window (get-buffer-window buffer))
              (select-window window)
            ;; If you have any preference over window management, you
            (pop-to-buffer buffer))))
    (xwidget-webkit-new-session url)))

;;;###autoload
(defun akirak-browse-url-external-browser (url &optional arg)
  "Browse URL using an external program."
  (unless (akirak-browse-url--non-default-p url arg)
    (browse-url-default-browser url arg)))

(defun akirak-browse-url--non-default-p (url &optional arg)
  "Return non-nil if a non-default browser is used."
  (let ((host (url-host (if (url-p url)
                            url
                          (url-generic-parse-url url)))))
    (cond
     ((akirak-browse-url-private-host-p host)
      (prog1 t
        (akirak-browse-url-private-url url arg)))
     ((string-match-p (rx-to-string `(and (?  (+ anything) ".")
                                          (or ,@akirak-browse-chromium-hosts)))
                      host)
      (prog1 t
        (akirak-browse-url-chromium-default url arg))))))

(defun akirak-browse-url-chromium-default (url &optional arg)
  (start-process "chromium"
                 nil
                 browse-url-chromium-program
                 ;; At present, only a single private profile is allowed.
                 ;; This may be changed in the future.
                 "--profile-directory=Default"
                 url))

(defun akirak-browse-url-private-url (url &optional arg)
  (start-process "chromium"
                 nil
                 browse-url-chromium-program
                 ;; At present, only a single private profile is allowed.
                 ;; This may be changed in the future.
                 "--profile-directory=Private"
                 url))

(defun akirak-browse-url-chromium-oneshot-advice (&rest _args)
  (advice-remove 'browse-url-chromium
                 #'akirak-browse-url-chromium-oneshot-advice)
  (setq browse-url-browser-function akirak-browser-url-orig-fn))

;;;###autoload
(defun akirak-browse-url-use-oneshot-chromium ()
  "Use Chromium for browsing the next url."
  (interactive)
  (setq akirak-browser-url-orig-fn browse-url-browser-function
        browse-url-browser-function #'browse-url-chromium)
  (advice-add 'browse-url-chromium
              :after #'akirak-browse-url-chromium-oneshot-advice))

(provide 'akirak-browse-url)
;;; akirak-browse-url.el ends here
