(asdf:defsystem :magitek
  :description "Whimsical robots infused with just a hint of magic."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (:losh
               :chirp
               :sqlite
               :iterate
               :cl-arrows)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "main")))))
