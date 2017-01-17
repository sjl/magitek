(asdf:defsystem :magitek
  :description "Whimsical robots infused with just a hint of magic."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (

               :chancery
               :chirp
               :cl-arrows
               :clss
               :drakma
               :flexi-streams
               :html-entities
               :iterate
               :jonathan
               :losh
               :named-readtables
               :plump
               :sanitize
               :split-sequence
               :sqlite

               )

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "database")
                             (:file "twitter")
                             (:file "markov")
                             (:module "robots"
                              :components ((:file "git-commands")
                                           (:file "hacker-booze")))
                             (:file "main")))))
