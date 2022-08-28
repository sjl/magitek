(in-package :magitek.markov)

;;;; Database API -------------------------------------------------------------
(defclass markov ()
  ((database :initarg :database :accessor markov-database
             :documentation "A mapping of {prefix #(suffix ...)}")
   (beginnings :initarg :beginnings :accessor markov-beginnings
               :documentation "A vector of all the prefixes beginning a sentence.")))

(defun make-markov ()
  (make-instance 'markov
    :database (make-hash-table :test 'equal)
    :beginnings (make-array 10 :fill-pointer 0 :adjustable t)))


(defun markov-insert-pair (markov prefix suffix)
  (vector-push-extend
    suffix
    (alexandria:ensure-gethash prefix (markov-database markov)
      (make-array 1 :fill-pointer 0 :adjustable t))))

(defun markov-insert-beginning (markov prefix)
  (vector-push-extend prefix (markov-beginnings markov)))


(defun markov-random-beginning (markov)
  (random-elt (markov-beginnings markov)))

(defun markov-random-suffix (markov prefix)
  (random-elt (gethash prefix (markov-database markov))))


;;;; Corpus-parsing & Building ------------------------------------------------
(defun delimiterp (c)
  (member c '(#\space #\newline) :test #'char=))

(defun sentence-end-p (word)
  (member (aref word (1- (length word)))
          '(#\. #\? #\!)))


(defun split-after (pred list)
  "Split `list` into sublists after each element satisfying `predicate`

  Example:

    (split-after #'zerop '(1 2 0 3 4 0 0 1))
    ; => ((1 2 0) (3 4 0) (0) (1))

  "
  (iterate (for (element . remaining) :on list)
           (collect element :into current)
           (when (funcall pred element)
             (collect current :into result)
             (setf current nil))
           (when (and (null remaining)
                      (not (null current)))
             (collect current :into result))
           (finally (return result))))

(defun split-before (pred list)
  "Split `list` into sublists before each element satisfying `predicate`

  Example:

    (split-before #'zerop '(1 2 0 3 4 0 0 1))
    ; => ((1 2) (0 3 4) (0) (0 1))

  "
  (iterate (for (element . remaining) :on list)
           (when (funcall pred element)
             (collect current :into result)
             (setf current nil))
           (collect element :into current)
           (when (and (null remaining)
                      (not (null current)))
             (collect current :into result))
           (finally (return result))))


(defun split-words (string)
  (split-sequence:split-sequence-if #'delimiterp string
                                    :remove-empty-subseqs t))

(defun split-sentences (string)
  (split-after #'sentence-end-p (split-words string)))


(defun prefix (n-gram)
  (butlast n-gram))

(defun suffix (n-gram)
  (car (last n-gram)))


(defun n-grams (n sequence)
    "Find all `n`-grams of the sequence `sequence`."
    ;;; From quickutil
    (assert (and (plusp n)
                 (<= n (length sequence))))

    (etypecase sequence
      ;; Lists
      (list (loop :repeat (1+ (- (length sequence) n))
                  :for seq :on sequence
                  :collect (take n seq)))

      ;; General sequences
      (sequence (loop :for i :to (- (length sequence) n)
                      :collect (subseq sequence i (+ i n))))))

(defun chunk-sentence (size sentence)
  (mapcar (juxt #'prefix #'suffix)
          (when (>= (length sentence) size)
            (n-grams size sentence))))


(defun build-markov-generator (corpus order)
  (iterate
    (with markov = (make-markov))
    (for sentence :in (split-sentences corpus))
    (iterate
      (for (prefix suffix) :in (chunk-sentence (1+ order) sentence))
      (if-first-time
        (markov-insert-beginning markov prefix))
      (markov-insert-pair markov prefix suffix))
    (finally (return markov))))


;;;; Generation ---------------------------------------------------------------
(defun generate-sentence (markov)
  (iterate
    (repeat 50)
    (with start = (markov-random-beginning markov))
    (if-first-time (appending start :into sentence))
    (for prefix :first start :then (append (cdr prefix) (list word)))
    (for word = (markov-random-suffix markov prefix))
    (collect word :into sentence)
    (until (sentence-end-p word))
    (finally (return (format nil "~{~A~^ ~}" sentence)))))


;;;; Scratch ------------------------------------------------------------------
