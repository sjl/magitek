(defpackage :magitek.twitter
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :magitek.quickutils)
  (:export
    :tt-load-credentials
    :tt-authorize
    :tt-tweetable-p
    :tt-tweet))

(defpackage :magitek.database
  (:use
    :cl
    :losh
    :sqlite
    :magitek.quickutils)
  (:export
    :db-connect
    :db-insert-tweet
    :db-tweeted-since-p))


(defpackage :magitek.robots.git-commands
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :chancery
    :magitek.quickutils)
  (:export :random-string))


(defpackage :magitek
  (:use
    :cl
    :iterate
    :cl-arrows
    :losh
    :magitek.twitter
    :magitek.database
    :magitek.quickutils)
  (:export
    :main))
