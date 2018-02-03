(defpackage :magitek.twitter
  (:use
    :cl
    :iterate
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
    :magitek.quickutils)
  (:export
    :build-markov-generator
    :generate-sentence))

(defpackage :magitek.diamond-square
  (:use
    :cl
    :losh
    :iterate
    :magitek.quickutils)
  (:export
    :generate-heightmap))


(defpackage :magitek.robots.git-commands
  (:use
    :cl
    :losh
    :chancery
    :magitek.quickutils)
  (:export :random-string))

(defpackage :magitek.robots.lisp-talks
  (:use
    :cl
    :losh
    :chancery
    :magitek.quickutils)
  (:export :random-string))

(defpackage :magitek.robots.rpg-shopkeeper
  (:use
    :cl
    :losh
    :chancery
    :magitek.quickutils)
  (:export :random-string))

(defpackage :magitek.robots.frantic-barista
  (:use
    :cl
    :losh
    :chancery
    :magitek.quickutils)
  (:export :random-string))


(defpackage :magitek
  (:use
    :cl
    :iterate
    :losh
    :magitek.twitter
    :magitek.database
    :magitek.quickutils)
  (:export
    :main))
