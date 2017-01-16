(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-gethash
               :ensure-list
               :n-grams
               :once-only
               :rcurry
               :read-file-into-string
               :with-gensyms

               )
  :package "MAGITEK.QUICKUTILS")
