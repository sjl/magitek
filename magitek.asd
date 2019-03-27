(asdf:defsystem :magitek
  :description "Whimsical robots infused with just a hint of magic."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (

               :chancery
               :chirp
               :fare-quasiquote
               :fare-quasiquote-readtable
               :flax
               :iterate
               :losh
               :named-readtables
               :split-sequence
               :sqlite
               :trivia
               :trivia.quasiquote
               :cl-netpbm

               )

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "magitek"
  :entry-point "magitek:main"

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "readtables")
                             (:file "database")
                             (:file "twitter")
                             (:file "markov")
                             (:file "diamond-square")
                             (:module "robots"
                              :components ((:file "git-commands")
                                           (:file "lisp-talks")
                                           (:file "bit-loom")
                                           (:file "frantic-barista")
                                           (:file "rpg-shopkeeper")))
                             (:file "main")))))
