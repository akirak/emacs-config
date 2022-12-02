;;; akirak-wordnet.el --- WordNet interface -*- lexical-binding: t -*-

(require 'emacsql-sqlite)

(defcustom akirak-wordnet-database-file
  (expand-file-name "dict/wordnet.db" (xdg-data-home))
  ""
  :type 'file)

(defmacro akirak-wordnet-with-database (&rest body)
  `(let ((conn (emacsql-sqlite akirak-wordnet-database-file)))
     (unwind-protect
         (progn ,@body)
       (emacsql-close conn))))

(defun akirak-wordnet-run-statement (statement)
  (with-temp-buffer
    (pcase (call-process "sqlite3" nil (list t nil) nil
                         (expand-file-name akirak-wordnet-database-file)
                         statement)
      (0 (thread-last
           (split-string (buffer-string) "\n")
           (mapcar (lambda (s)
                     (split-string s "|")))
           (cl-remove-if (lambda (xs)
                           (string-empty-p (car xs))))))
      (i (error "sqlite3 failed with %d" i)))))

;;;###autoload
(defun akirak-wordnet-word-derivations (word)
  "Return derivationally related forms of a word."
  (akirak-wordnet-with-database
   (cl-flet
       ((find-word (word1)
          (thread-last
            (emacsql conn
                     [:select [w2:writtenform]
                              :from word w1
                              :inner-join lexrel rel
                              :on (= w1:wordno rel:wordno1)
                              :inner-join reltype
                              :on (= rel:reltypeno reltype:reltypeno)
                              :inner-join word w2
                              :on (= rel:wordno2 w2:wordno)
                              :where (= reltype:reltypename
                                        '"DERIVATIONALLY RELATED FORM")
                              :and (= w1:writtenform $r1)
                              :group-by [w2:writtenform]]
                     word1)
            (mapcar #'car)
            (mapcar #'symbol-name))))
     (or (find-word word)
         (save-match-data
           (when (string-match (rx bol (group (+? anything)) (or "s" "ly"))
                               word)
             (find-word (match-string 1 word))))))))

;;;###autoload
(defun akirak-wordnet-lexnames (word)
  (akirak-wordnet-with-database
   (thread-last
     (emacsql conn
              [:select lexname:lexname
                       :from word
                       :inner-join sense
                       :on (= sense:wordno word:wordno)
                       :inner-join synset
                       :on (= sense:synsetno synset:synsetno)
                       :inner-join lexname
                       :on (= synset:lexfilenum lexname:lexfilenum)
                       :where (= word:writtenform $r1)
                       :group-by synset:lexfilenum]
              word)
     (mapcar #'car)
     (mapcar #'symbol-name))))

(defconst akirak-wordnet-synonym-sql
  "select ss1.definition, reltype.speech, reltype.reltypename, w2.writtenform \
  from word w1 \
  inner join sense s1 \
  on s1.wordno = w1.wordno \
  inner join synset ss1 \
  on s1.synsetno = ss1.synsetno \
  inner join semrel \
  on semrel.synsetno1 = s1.synsetno \
  inner join reltype \
  on semrel.reltypeno = reltype.reltypeno \
  inner join sense s2 \
  on s2.synsetno = semrel.synsetno2 \
  inner join word w2 \
  on w2.wordno = s2.wordno \
  where w1.writtenform = '%s'\
  group by reltype.reltypeno;")

;;;###autoload
(defun akirak-wordnet-thesaurus (word)
  (akirak-wordnet-run-statement
   (format akirak-wordnet-synonym-sql word)))

(defun akirak-wordnet--thesaurus-taxy (word)
  (thread-last
    (make-taxy
     :taxys (list
             (make-taxy
              :take (apply-partially
                     #'taxy-take-keyed (list (lambda (x) (nth 0 x))
                                             (lambda (x)
                                               (list (nth 1 x)
                                                     (nth 2 x))))))))
    (taxy-emptied)
    (taxy-fill (akirak-wordnet-thesaurus word))
    (taxy-mapcar (lambda (record) (car (last record))))
    (taxy-plain)
    (caaar)))

;;;###autoload
(defun org-dblock-write:wordnet (params)
  (require 'taxy)

  (let* ((word (or (plist-get params :word)
                   (org-get-heading t t t t)))
         (word (cl-etypecase word
                 (symbol (symbol-name word))
                 (string word))))
    (pcase-dolist (`(,definition ,branches) (akirak-wordnet--thesaurus-taxy word))
      (insert "- " definition "\n")
      (pcase-dolist (`(,relationship ,entries) branches)
        (insert (make-string 2 ?\s)
                "- "
                (if (string-match-p "^[[:blank:]]+$" (car relationship))
                    ""
                  (format "(%s) " (car relationship)))
                (cadr relationship)
                " :: "
                (mapconcat (lambda (x) (format "[[%s]]" x))
                           entries
                           ", ")
                "\n")))
    (backward-delete-char 1)))

(provide 'akirak-wordnet)
;;; akirak-wordnet.el ends here
