(in-package :magitek)

(defclass* bot () (name generator hours))

(defun make-bot (name generator hours)
  (make-instance 'bot :name name :generator generator :hours hours))


(define-with-macro bot
  name generator hours)


(defparameter *git-commands*
  (make-bot :git-commands
            #'magitek.robots.git-commands:random-tweet
            12))

(defparameter *lisp-talks*
  (make-bot :lisp-talks
            #'magitek.robots.lisp-talks:random-tweet
            12))

(defparameter *rpg-shopkeeper*
  (make-bot :rpg-shopkeeper
            #'magitek.robots.rpg-shopkeeper:random-tweet 
            12))

(defparameter *frantic-barista*
  (make-bot :frantic-barista
            #'magitek.robots.frantic-barista:random-tweet
            6))

(defparameter *bit-loom*
  (make-bot :bit-loom
            #'magitek.robots.bit-loom:random-tweet
            5))


(defun hours-to-minutes (h)
  (* h 60))


(defun generate-tweet (generator)
  (do-repeat 100
    (multiple-value-bind (text media) (funcall generator)
      (when (tt-tweetable-p text)
        (return (values text media))))))


(defun run-bot (bot &key (force nil) (dry t))
  (with-bot (bot)
    (format t "Running ~S~%" name)
    (when (or force
              (not (db-tweeted-since-p name (hours-to-minutes hours))))
      (multiple-value-bind (tweet media) (generate-tweet generator)
        (if (null tweet)
          (format t "Could not generate a suitable tweet for ~S~%" name)
          (progn
            (format t "Tweeting as ~S (media ~S): ~S~%" name media tweet)
            (db-insert-tweet name tweet)
            (if dry
              (format t "Skipping actual tweet (dry run).")
              (progn
                (tt-tweet name tweet media)
                (sleep 5.0)))))))))


(defun spinup ()
  (db-connect)
  (db-initialize)
  (tt-load-credentials))

(defun main ()
  (setf *random-state* (make-random-state t))
  (spinup)
  (run-bot *frantic-barista* :dry nil)
  (run-bot *git-commands* :dry nil)
  (run-bot *lisp-talks* :dry nil)
  (run-bot *rpg-shopkeeper* :dry nil)
  (run-bot *bit-loom* :dry nil))
