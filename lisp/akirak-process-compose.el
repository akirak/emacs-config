;;; akirak-process-compose.el ---  -*- lexical-binding: t -*-

(require 'map)

(defcustom akirak-process-compose-yq-executable "yq"
  ""
  :type 'file)

(defun akirak-process-compose-find-config (dir)
  (catch 'found-process-compose
    (dolist (file '("process-compose.yaml"
                    "process-compose.yml"
                    "process-compose.json"))
      (let ((path (file-name-concat dir file)))
        (when (file-exists-p path)
          (throw 'found-process-compose path))))))

(defun akirak-process-compose-parse (file)
  (with-temp-buffer
    (insert-file-contents file)
    (when (string-match-p (rx (or ".yaml" ".yml") eol)
                          file)
      (unless (zerop (call-process-region (point-min) (point-max)
                                          akirak-process-compose-yq-executable
                                          t (list t nil) nil
                                          "." "-"))
        (error "Failed to parse the file using yq: %s" file)))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'hash-table :array-type 'list)))

(defun akirak-process-compose-process-names (config)
  (thread-first
    (map-elt config "processes")
    (map-keys)))

(provide 'akirak-process-compose)
;;; akirak-process-compose.el ends here
