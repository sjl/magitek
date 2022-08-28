(defpackage :magitek.twitter
  (:use
    :cl
    :iterate
    :losh)
  (:export
    :tt-load-credentials
    :tt-authorize
    :tt-tweetable-p
    :tt-tweet))

(defpackage :magitek.database
  (:use
    :cl
    :losh
    :sqlite)
  (:export
    :db-connect
    :db-initialize
    :db-insert-tweet
    :db-tweeted-since-p))


(defpackage :magitek.markov
  (:use
    :cl
    :losh
    :iterate)
  (:export
    :build-markov-generator
    :generate-sentence))

(defpackage :magitek.diamond-square
  (:use
    :cl
    :losh
    :iterate)
  (:export
    :generate-heightmap))


(defpackage :magitek.robots.bit-loom
  (:use
    :cl
    :losh)
  (:export :random-tweet))

(defpackage :magitek.robots.git-commands
  (:use
    :cl
    :losh
    :chancery)
  (:export :random-tweet))

(defpackage :magitek.robots.lisp-talks
  (:use
    :cl
    :losh
    :chancery)
  (:export :random-tweet))

(defpackage :magitek.robots.rpg-shopkeeper
  (:use
    :cl
    :losh
    :chancery)
  (:export :random-tweet))

(defpackage :magitek.robots.frantic-barista
  (:use
    :cl
    :losh
    :chancery)
  (:export :random-tweet))


(defpackage :magitek
  (:use
    :cl
    :iterate
    :losh
    :magitek.twitter
    :magitek.database)
  (:export
    :main))
