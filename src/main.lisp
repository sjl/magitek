(in-package :magitek)

(defun hours-to-minutes (h)
  (* h 60))


(defun generate-tweet (generator)
  (iterate
    (repeat 100)
    (finding (funcall generator) :such-that #'tt-tweetable-p)))


(defun run-bot (name generator &key (hours 12))
  (format t "Running ~S~%" name)
  (when (not (db-tweeted-since-p name (hours-to-minutes hours)))
    (let ((tweet (generate-tweet generator)))
      (if (null tweet)
        (format t "Could not generate a suitable tweet for ~S~%" name)
        (progn
          (format t "Tweeting as ~S: ~S~%" name tweet)
          (db-insert-tweet name tweet)
          (tt-tweet name tweet)
          (sleep 5.0))))))


(defun main ()
  (db-connect)
  (db-initialize)
  (tt-load-credentials)
  (run-bot :git-commands #'magitek.robots.git-commands:random-string
           :hours 12))
