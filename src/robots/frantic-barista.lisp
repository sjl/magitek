(in-package :magitek.robots.frantic-barista)
(named-readtables:in-readtable :chancery)

;;;; Utils --------------------------------------------------------------------
(defun english-number (n)
  (format nil "~R" n))


;;;; Names --------------------------------------------------------------------
(define-string name-simple
  "Sophia"
  "Jackson"
  "Emma"
  "Aiden"
  "Olivia"
  "Lucas"
  "Ava"
  "Liam"
  "Mia"
  "Noah"
  "Isabella"
  "Ethan"
  "Riley"
  "Mason"
  "Aria"
  "Caden"
  "Zoe"
  "Oliver"
  "Charlotte"
  "Elijah"
  "Lily"
  "Grayson"
  "Layla"
  "Jacob"
  "Amelia"
  "Michael"
  "Emily"
  "Benjamin"
  "Madelyn"
  "Carter"
  "Aubrey"
  "James"
  "Adalyn"
  "Jayden"
  "Madison"
  "Logan"
  "Chloe"
  "Alexander"
  "Harper"
  "Caleb")

(define-string name-twitter
  "@stevelosh"
  "@fu86"
  "@rpg_shopkeeper"
  "@git_commands")

(define-string (name :distribution :weighted)
  (50 name-simple)
  (1 name-twitter))


;;;; Liquids ------------------------------------------------------------------
(define-string liquid
  "coffee"
  "cappuccino"
  "mocha"
  "latte"
  "espresso"
  "tea"
  "chai"
  "hot chocolate")


;;;; Sizes --------------------------------------------------------------------
(define-string size-simple
  "small"
  "medium"
  "large")

(define-string size-pretentious
  "short"
  "tall"
  "grande"
  "venti")

(define-string (size :distribution :weighted)
  (5 size-simple)
  (1 size-pretentious))


;;;; Flavor -------------------------------------------------------------------
(define-string adjective
  "artisanal"
  "hand-crafted"
  "fresh"
  "decaf"
  "day-old")

(define-string topping
  "whipped cream"
  "cinnamon"
  "sprinkles")


;;;; Main ---------------------------------------------------------------------
(define-string drink-base
  liquid
  (size liquid)
  (adjective size liquid)
  (adjective liquid))

(define-string drink
  drink-base
  (drink-base "with" topping))

(define-string order
  ("I've got" [drink a] "for" name "here!")
  ([!(random-range-inclusive 2 9) english-number cap]
   [drink-base s] "under" name :. ", to go!"))


;;;; API ----------------------------------------------------------------------
(defun random-string ()
  (order))

