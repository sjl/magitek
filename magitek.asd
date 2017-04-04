(asdf:defsystem :magitek
  :description "Whimsical robots infused with just a hint of magic."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (

               :chancery
               :chirp
               :clss
               :drakma
               :fare-quasiquote
               :fare-quasiquote-readtable
               :flexi-streams
               ;; :html-entities ; fuck my life
               :iterate
               :jonathan
               :losh
               :named-readtables
               :plump
               :sanitize
               :split-sequence
               :sqlite
               :trivia
               :trivia.quasiquote

               )

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
                             (:module "robots"
                              :components ((:file "git-commands")
                                           (:file "lisp-talks")
                                           (:file "hacker-booze")
                                           (:file "frantic-barista")
                                           (:file "rpg-shopkeeper")))
                             (:file "main")))))
