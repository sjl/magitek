(in-package :magitek.robots.hacker-booze)

; https://www.youtube.com/watch?v=2eIFeTn5nJg

;;;; Utils --------------------------------------------------------------------
(defun tick (ch)
  (write-char ch)
  (finish-output))

(defun read-corpus (path)
  (read-file-into-string path))

(defun write-corpus (corpus path)
  (write-string-into-file corpus path
                          :if-exists :supersede))


;;;; Hacker News --------------------------------------------------------------
(defparameter *errors* 0)
(defparameter *stories-per-corpus* 30)
(defparameter *max-comments-per-story* 200)
(defparameter *hn-corpus-path* "corpora/hacker-news.txt")

(defun firebase-get (url)
  (-<> url
    drakma:http-request
    (flex:octets-to-string <> :external-format :utf-8)
    (jonathan:parse <> :as :hash-table)))


(defun hn-top ()
  (firebase-get "https://hacker-news.firebaseio.com/v0/topstories.json"))

(defun hn-item (id)
  (firebase-get
    (format nil "https://hacker-news.firebaseio.com/v0/item/~d.json" id)))


(defun hn-story (story-id)
  (hn-item story-id))

(defun hn-comment (story-id)
  (hn-item story-id))

(defun hn-text (comment)
  (-> (gethash "text" comment)
    sanitize:clean
    ;; this is gone from quicklisp because god hates me
    ;; html-entities:decode-entities
    ))


(defun hn-comments (story-id)
  (iterate
    (with story = (hn-story story-id))
    (with children = (gethash "kids" story))
    (repeat *max-comments-per-story*)
    ; (sleep 0.1)
    (while children)
    (for child-id = (pop children))
    (for child = (handler-case (hn-comment child-id)
                   (drakma::drakma-simple-error () (incf *errors*) nil)))
    (if child
      (progn
        (tick #\.)
        (collect child)
        (setf children (append children (gethash "kids" child))))
      (tick #\x))
    (finally (terpri))))


(defun rebuild-hn-corpus ()
  (write-corpus (-<> (hn-top)
                  (take *stories-per-corpus* <>)
                  (mapcan #'hn-comments <>)
                  (remove-if-not #'identity <>)
                  (mapcar #'hn-text <>)
                  (format nil "~{~a~%~}" <>))
                *hn-corpus-path*)
  (values))

(defun read-hn-corpus ()
  (read-corpus *hn-corpus-path*))


;;;; Beer ---------------------------------------------------------------------
(defparameter *ratebeer-pages* 5)
(defparameter *ratebeer-corpus-path* "corpora/ratebeer.txt")


(clss:define-pseudo-selector no-class (node)
  (null (plump:attribute node "class")))

(defun ratebeer-get (page)
  (-<> (format nil "http://www.ratebeer.com/beer-ratings/0/~d/" page)
    drakma:http-request
    plump:parse))

(defun ratebeer-clean (raw)
  "Return a list of review strings."
  (-<> raw
    (clss:select "table.table td > span:no-class" <>)
    (map 'list #'plump:text <>)))


(defun rebuild-ratebeer-corpus ()
  (write-corpus (iterate
                  (for page :from 1 :to *ratebeer-pages*)
                  (appending (ratebeer-clean (ratebeer-get page)) :into reviews)
                  (tick #\.)
                  (finally (return (format nil "~{~A~%~}" reviews))))
                *ratebeer-corpus-path*)
  (values))

(defun read-ratebeer-corpus ()
  (read-corpus *ratebeer-corpus-path*))


;;;; Wine ---------------------------------------------------------------------
(defparameter *wine-pages* 20)
(defparameter *wine-corpus-path* "corpora/wine.txt")


(defun wine-get-list (page-number)
  (-<> (format nil "http://www.winemag.com/?s=&drink_type=wine&page=~D"
               page-number)
    drakma:http-request
    plump:parse))

(defun wine-get-review (url)
  (-<> url
    drakma:http-request
    plump:parse))


(defun wine-clean-list (list-page)
  (-<> list-page
    (clss:select "a.review-listing" <>)
    (map 'list (rcurry #'plump:attribute "href") <>)))

(defun wine-clean-review (review-page)
  (-<> review-page
    (clss:select "#review .description" <>)
    (elt <> 0)
    (plump:text <>)))


(defun wine-get-reviews (page)
  (iterate
    (for review-link :in (wine-clean-list (wine-get-list page)))
    (collect (wine-clean-review (wine-get-review review-link)))
    (tick #\.)
    (finally (terpri))))


(defun rebuild-wine-corpus ()
  (write-corpus (iterate
                  (for page :from 1 :to *wine-pages*)
                  (appending (wine-get-reviews page) :into reviews)
                  (finally (return (format nil "~{~A~%~}" reviews))))
                *wine-corpus-path*)
  (values))

(defun read-wine-corpus ()
  (read-corpus *wine-corpus-path*))


;;;; Generate -----------------------------------------------------------------
(defparameter *markov* nil)
(defparameter *markov-order* 2)


(defun load-corpora ()
  (setf *markov*
        (magitek.markov:build-markov-generator
          (concatenate 'string
                       (read-wine-corpus)
                       (read-hn-corpus))
          *markov-order*))
  (values))


(defun random-string ()
  (magitek.markov:generate-sentence *markov*))
