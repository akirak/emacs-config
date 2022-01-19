;;; init.el ---  -*- lexical-binding: t; no-byte-compile: t -*-

(require 'org)
(org-babel-load-file
 (expand-file-name "emacs-config.org"
                   (file-name-directory (or load-file-name
                                            (buffer-file-name)))))
