(in-package :magitek)

(defclass* bot () (name generator hours))

(defun make-bot (name generator hours)
  (make-instance 'bot :name name :generator generator :hours hours))


(define-with-macro bot
  name generator hours)


(defparameter *git-commands*
  (make-bot :git-commands
            #'magitek.robots.git-commands:random-string
            12))

(defparameter *lisp-talks*
  (make-bot :lisp-talks
            #'magitek.robots.lisp-talks:random-string
            12))

(defparameter *rpg-shopkeeper*
  (make-bot :rpg-shopkeeper
            #'magitek.robots.rpg-shopkeeper:random-string 
            12))

(defparameter *frantic-barista*
  (make-bot :frantic-barista
            #'magitek.robots.frantic-barista:random-string
            6))


(defun hours-to-minutes (h)
  (* h 60))


(defun generate-tweet (generator)
  (iterate
    (repeat 100)
    (finding (funcall generator) :such-that #'tt-tweetable-p)))


(defun run-bot (bot &key (force nil) (dry nil))
  (with-bot (bot)
    (format t "Running ~S~%" name)
    (when (or force
              (not (db-tweeted-since-p name (hours-to-minutes hours))))
      (let ((tweet (generate-tweet generator)))
        (if (null tweet)
          (format t "Could not generate a suitable tweet for ~S~%" name)
          (progn
            (format t "Tweeting as ~S: ~S~%" name tweet)
            (db-insert-tweet name tweet)
            (unless dry
              (tt-tweet name tweet)
              (sleep 5.0))))))))


(defun spinup ()
  (db-connect)
  (db-initialize)
  (tt-load-credentials))

(defun main ()
  (spinup)
  (run-bot *frantic-barista*)
  (run-bot *git-commands*)
  (run-bot *lisp-talks*)
  (run-bot *rpg-shopkeeper*))
