(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :compose
               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :n-grams
               :once-only
               :rcurry
               :read-file-into-string
               :symb
               :with-gensyms
               :write-string-into-file

               )
  :package "MAGITEK.QUICKUTILS")
