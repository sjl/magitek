(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-gethash
               :n-grams
               :once-only
               :rcurry
               :symb
               :with-gensyms

               )
  :package "MAGITEK.QUICKUTILS")
