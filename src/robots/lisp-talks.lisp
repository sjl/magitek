(in-package :magitek.robots.lisp-talks)
(named-readtables:in-readtable :chancery)

;; https://twitter.com/lisp_talks

(defun ? (arg)
  (if (randomp)
    (format nil "~A " arg)
    ""))


(define-string (dialect :distribution :weighted)
  (5 "Arc")
  (5 "Clojurescript")
  (5 "Picolisp")
  (1 "Emacs Lisp")
  (2 "Lisp-Flavored Erlang")
  (10 "Common Lisp")
  (10 "Clojure")
  (10 "Scheme"))

(define-string implementation
  "Racket"
  "Guile"
  "Chicken"
  "Clasp"
  "CLISP"
  "SBCL"
  "ABCL"
  "ECL"
  "CCL")

(define-string language
  implementation
  dialect)

(define-string noun
  "source file"
  "type"
  "form"
  "image"
  "coffee"
  "archive"
  "macro"
  "number"
  "float"
  "class"
  "struct"
  "lambda"
  "compiler macro"
  "sequence"
  "HyperSpec"
  "UltraSpec"
  "network protocol"
  "sufficiently-smart compiler"
  "m-expression"
  "s-expression")

(define-string editor
  "Emacs"
  "Vim"
  "Neovim"
  "Sublime Text"
  "Atom"
  "Notepad"
  "Notepad++"
  "Eclipse"
  "SLIME"
  "NREPL"
  "CIDER"
  "ed"
  "acme"
  "nano"
  "vi"
  "IntelliJ")

(define-string editor-or-language
  editor
  language)

(define-string people
  "Lispers"
  "Schemers"
  "Clojurists"
  "Rich Hickey"
  "the X3J13 committee"
  "teachers"
  "programmers"
  "engineers"
  "students"
  "undergrads"
  "grad students"
  "PhD students")

(define-string activitying
  "evaluating"
  "expanding"
  "refactoring"
  "reading"
  "compiling"
  "recompiling"
  "serializing"
  "typechecking"
  "parsing")

(define-string activityion
  "evaluation"
  "expansion"
  "compilation"
  "recompilation"
  "serialization")

(define-string doer
  "evaluator"
  "expander"
  "reader"
  "compiler"
  "recompiler"
  "serializer"
  "typechecker"
  "parser")

(define-string making
  "writing"
  "making"
  "creating")

(define-string adjective
  "faster"
  "better"
  "portable"
  "easy"
  "simple"
  "safer"
  "complected"
  "extensible"
  "strongly-typed"
  "maintainable"
  "interactive"
  "saner"
  (language :. "-specific"))

(define-string techniques
  "techniques"
  "strategies")


(define-string talk-demo
  ("Paredit for" editor-or-language)
  ("a library for" activitying [noun s] "in" language)
  ("a portable" noun "library for" dialect)
  (activitying [noun s] "in" editor))

(define-string talk-parallel
  (["efficiently" ?] :. "parallelizing" activityion "of" [noun s])
  (["efficiently" ?] :. "parallelizing" activityion "of" [noun s] "in" language))

(define-string talk-teaching
  ("teaching" people adjective activitying techniques)
  ("a masterclass in" making [doer s])
  ("how to teach" people adjective activityion techniques)
  ("lessons learned from a class in" [activitying ?] :. [noun s]))

(define-string talk-better
  (adjective :. "," adjective :. ", and" adjective [noun s])
  ("writing" adjective :. "," adjective language))

(define-string talk-on
  ("on" [noun s])
  ("on" editor-or-language)
  ("on" people))

(define-string talk-writing
  ("writing" adjective dialect)
  ("writing" [adjective a] doer)
  ("writing" adjective [doer s])
  ("writing" [adjective a] doer "in" editor-or-language))

(define-string talk-tricks
  ("stupid" editor "tricks")
  ("stupid" doer "tricks")
  ("one weird trick for" adjective activityion "in" editor-or-language))

(define-string talk-x-in-y
  ("managing" [noun s] "in" language)
  ("dealing with" [noun s] "in" language)
  ("dealing with" activityion "in" language)
  ("DIY" noun :. "-" :. activitying "in" editor-or-language))

(define-string talk-editing
  ("editing" [adjective ?] :. dialect "faster with" editor))


(define-string talk%
  ("demo:" [talk-demo cap])
  talk-editing
  talk-x-in-y
  talk-writing
  talk-on
  talk-teaching
  talk-parallel
  talk-better)

(define-string (talk :distribution :weighted)
  (50 talk%)
  ( 1 ("reflections on" [talk% cap]))
  ( 1 (talk% "--" !(+ 5 (random 40)) "years later"))
  ( 1 (talk% :. ", Part" !(format nil "~@R" (+ 1 (random 3))))))


;;;; API ----------------------------------------------------------------------
(defun random-tweet ()
  (cap (talk)))

