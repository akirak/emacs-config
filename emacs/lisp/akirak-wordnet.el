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

;;;###autoload
(defun akirak-wordnet-word-derivations (word)
  "Return derivationally related forms of a word."
  (akirak-wordnet-with-database
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
              word)
     (mapcar #'car)
     (mapcar #'symbol-name))))

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

(provide 'akirak-wordnet)
;;; akirak-wordnet.el ends here
