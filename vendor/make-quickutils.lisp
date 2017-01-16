(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :curry
               :ensure-boolean
               :ensure-list
               :once-only
               :rcurry
               :with-gensyms

               )
  :package "MAGITEK.QUICKUTILS")
