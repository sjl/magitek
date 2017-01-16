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
    :db-initialize
    :db-insert-tweet
    :db-tweeted-since-p))


(defpackage :magitek.markov
  (:use
    :cl
    :losh
    :iterate
    :cl-arrows
    :magitek.quickutils)
  (:export
    ))


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
