(in-package :magitek.robots.rpg-shopkeeper)
(named-readtables:in-readtable :nutbags-readtable)

; https://twitter.com/rpg_shopkeeper

;;;; Utils --------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun muffenize (str)
    (symb '* (string-upcase (substitute #\- #\space str)) '*)))

(defmacro maybe (expr &optional (chance 0.5))
  `(when (randomp ,chance)
     ,expr))


;;;; General ------------------------------------------------------------------
(define-rule (bonus :distribution (:zipf :exponent 1.8))
  1 2 3 4 5)


;;;; Monsters -----------------------------------------------------------------
(defclass* (monster :conc-name monster-) ()
  (singular multiplier plural adjective))

(defun make-monster (singular multiplier plural adjective)
  (make-instance 'monster
    :singular singular
    :multiplier multiplier
    :plural plural
    :adjective adjective))

(defmethod print-object ((o monster) s)
  (print-unreadable-object (o s :type t)
    (princ (monster-singular o) s)))

(defmacro define-monster (singular multiplier plural adjective)
  `(defparameter ,(muffenize singular)
     (make-monster
       ,singular
       ,multiplier
       ,plural
       ,adjective)))


(define-monster "goblin"   1.00 "goblins"   "goblin")
(define-monster "kobold"   1.00 "kobolds"   "kobold")
(define-monster "elf"      1.00 "elves"     "elven")
(define-monster "dwarf"    1.00 "dwarves"   "dwarven")
(define-monster "halfling" 1.00 "halflings" "halfling")
(define-monster "gnome"    1.00 "gnomes"    "gnomish")
(define-monster "gnoll"    1.10 "gnolls"    "gnollish")
(define-monster "ogre"     1.20 "ogres"     "ogre")
(define-monster "troll"    1.60 "trolls"    "troll")
(define-monster "vampire"  5.00 "vampires"  "vampire")
(define-monster "dragon"   9.00 "dragons"   "dragon")


(define-rule (monster :distribution :weighted)
  (1.00 !*goblin*)
  (1.00 !*kobold*)
  (1.00 !*elf*)
  (1.00 !*dwarf*)
  (1.00 !*halfling*)
  (1.00 !*gnome*)
  (1.00 !*ogre*)
  (1.00 !*troll*)
  (1.00 !*gnoll*)
  (0.10 !*vampire*)
  (0.01 !*dragon*))


;;;; Materials ----------------------------------------------------------------
(defclass* (material :conc-name material-) ()
  (kind name multiplier))

(defun make-material (kind name multiplier)
  (make-instance 'material :kind kind :name name :multiplier multiplier))

(defmethod print-object ((o material) s)
  (print-unreadable-object (o s :type t)
    (princ (material-name o) s)))

(defmacro define-material (kind name multiplier)
  `(defparameter ,(muffenize name)
     (make-material ,kind ,name ,multiplier)))


(define-material :metal "iron" 1.0)
(define-material :metal "steel" 1.5)
(define-material :metal "silver" 5.0)
(define-material :metal "meteoric iron" 10.0)
(define-material :metal "mithril" 50.0)
(define-material :metal "adamantine" 60.0)
(define-material :textile "cloth" 1.0)
(define-material :textile "leather" 1.5)
(define-material :textile "silk" 2.0)
(define-material :textile "spider silk" 6.0)
(define-material :wood "pine" 1.0) ; http://www.wood-database.com/wood-articles/bow-woods/
(define-material :wood "poplar" 1.2)
(define-material :wood "walnut" 1.3)
(define-material :wood "maple" 2.0)
(define-material :wood "oak" 3.0)
(define-material :wood "yew" 5.0)
(define-material :wood "rosewood" 10.0)
(define-material :skin "leather" 1.0)


(define-rule (metal :distribution :zipf)
  !*iron*
  !*steel*
  !*silver*
  !*meteoric-iron*
  !*mithril*
  !*adamantine*)

(define-rule (textile :distribution :weighted)
  (1.00 !*cloth*)
  (0.80 !*silk*)
  (0.05 !*spider-silk*))

(define-rule (wood :distribution (:zipf :exponent 0.8))
  !*pine*
  !*poplar*
  !*walnut*
  !*maple*
  !*oak*
  !*yew*
  !*rosewood*)

(define-rule (skin :distribution :weighted)
  (3 !*leather*)
  (1 !(let ((monster (monster)))
        (make-material :skin
                       $(!(monster-singular monster) :. "skin")
                       (1+ (monster-multiplier monster))))))


(define-string material-picture-wood
  "carved with images of"
  "covered in carvings of"
  "painted with pictures of")

(define-string material-picture-metal
  "engraved with an image of"
  "etched with a picture of"
  "painted with images of")

(define-string material-picture-textile
  "sewn with pictures of"
  "embroidered with images of")

(define-string material-picture-skin
  "dyed with images of"
  "sewn with pictures of"
  "embossed with drawings of")


(defun material-picture (material)
  (ecase (material-kind material)
    (:metal (material-picture-metal))
    (:wood (material-picture-wood))
    (:textile (material-picture-textile))
    (:skin (material-picture-skin))))


;;;; Roles --------------------------------------------------------------------
(define-rule role
  "cleric"
  "warrior"
  "wizard"
  "bard"
  "jester"
  "priest"
  "hermit"
  "rogue")


;;;; Elements -----------------------------------------------------------------
(define-rule element
  ("good"      "holy")
  ("evil"      "unholy")
  ("ice"       "icy")
  ("fire"      "flaming")
  ("lightning" "electrified")
  ("poison"    "venomous"))


;;;; Pieces -------------------------------------------------------------------
(defclass* (piece :conc-name piece-) ()
  (name base-value))

(defmethod print-object ((o piece) stream)
  (print-unreadable-object (o stream :type t)
    (princ (piece-name o) stream)))

(defmacro define-piece (name base-value)
  `(defparameter ,(muffenize name)
     (make-instance 'piece
       :name ,name
       :base-value ,base-value)))


;;;; Ornaments ----------------------------------------------------------------
(define-string ornament-scene
  "a strange land"
  ([monster monster-plural] "fighting" [monster monster-plural])
  ("a famous" [monster monster-singular] role))

(define-string word-adjective
  "mysterious"
  "ancient"
  "glowing")

(define-string word-noun
  "runes"
  "symbols")

(define-string ornament-words
  ("covered in" word-adjective word-noun))

(define-rule (ornament :arguments (material))
  !$(!(material-picture material) ornament-scene)
  !$ornament-words
  nil)


;;;; Enchantments -------------------------------------------------------------
(define-rule enchant-armor
  (:protection monster)
  (:resistance element)
  (:bonus bonus))

(define-rule enchant-weapon
  (:slaying monster)
  (:element element)
  (:bonus bonus))


(defun enchantment-multiplier (enchantment)
  (declare (optimize (debug 3)))
  (ecase (first enchantment)
    ((nil) 1.0)
    ((:protection :slaying) (+ 2.0 (monster-multiplier (second enchantment))))
    ((:resistance :element) 5.0)
    (:bonus (expt 2.0 (second enchantment)))))


(defun enchanted-armor-description (base enchantment)
  (trivia:ematch enchantment
    (`(:protection ,monster)
     (format nil "~A of protection from ~A" base (monster-plural monster)))
    (`(:resistance (,element ,_))
     (format nil "~A of ~A resistance" base element))
    (`(:bonus ,val)
     (format nil "+~D ~A" val base))))

(defun enchanted-weapon-description (base enchantment)
  (trivia:ematch enchantment
    (`(:slaying ,monster)
     (format nil "~A of ~A-slaying" base (monster-singular monster)))
    (`(:element (,_ ,element))
     (format nil "~A ~A" element base))
    (`(:bonus ,val)
     (format nil "+~D ~A" val base))))


;;;; Armor --------------------------------------------------------------------
(defclass* (armor :conc-name armor-) ()
  (material piece enchantment ornament))

(define-with-macro armor
  material piece enchantment ornament)


(define-piece "scale mail" 50)
(define-piece "ring mail" 50)
(define-piece "chain mail" 150)
(define-piece "breastplate" 200)
(define-piece "plate mail" 350)

(define-rule (piece-armor :distribution (:zipf :exponent 0.6))
  !*scale-mail*
  !*ring-mail*
  !*chain-mail*
  !*breastplate*
  !*plate-mail*)


(define-piece "robe" 10)
(define-piece "shirt" 5)
(define-piece "pants" 5)
(define-piece "dress" 9)
(define-piece "skirt" 5)

(define-rule piece-clothing
  !*robe*
  !*shirt*
  !*pants*
  !*dress*
  !*skirt*)


(define-rule (vanilla-armor :distribution :weighted)
  (1 (metal piece-armor))
  (1 (textile piece-clothing))
  (0.2 (skin piece-clothing)))

(define-rule (armor% :distribution :weighted)
  (2 (vanilla-armor nil))
  (1 (vanilla-armor enchant-armor)))


(defun armor ()
  (destructuring-bind ((material piece) enchantment)
      (armor%)
    (make-instance 'armor
      :material material
      :piece piece
      :enchantment enchantment
      :ornament (ornament material))))


(defun armor-value (armor)
  (with-armor (armor)
    (* (+ (* (_ piece piece-base-value)
             (_ material material-multiplier))
          (if enchantment 100 0)
          (if ornament 10 0))
       (enchantment-multiplier enchantment)
       (if ornament 1.5 1.0))))


(defun vanilla-armor-description (vanilla-armor)
  (format nil "~A ~A"
          (_ vanilla-armor armor-material material-name)
          (_ vanilla-armor armor-piece piece-name)))


(defun armor-description (armor)
  (let ((vanilla-description (vanilla-armor-description armor))
        (enchantment (armor-enchantment armor))
        (ornament (armor-ornament armor)))
    (concatenate 'string
                 (if enchantment
                   (enchanted-armor-description vanilla-description
                                                enchantment)
                   vanilla-description)
                 (if ornament
                   (format nil ", ~A" ornament)
                   ""))))


; ;;;; Weapons ------------------------------------------------------------------
(defclass* (weapon :conc-name weapon-) ()
  (material piece enchantment ornament))

(define-with-macro weapon
  material piece enchantment ornament)


(define-piece "dagger" 5)
(define-piece "longsword" 50)
(define-piece "short sword" 20)
(define-piece "hand axe" 30)
(define-piece "battleaxe" 80)
(define-piece "spear" 20)
(define-piece "halberd" 80)
(define-piece "scythe" 50)
(define-piece "scimitar" 50)
(define-piece "lance" 70)
(define-piece "warhammer" 80)
(define-piece "staff" 5)
(define-piece "mace" 25)
(define-piece "flail" 45)
(define-piece "sling" 5)

(define-rule piece-melee
  !*dagger*
  !*longsword*
  !*short-sword*
  !*hand-axe*
  !*battleaxe*
  !*spear*
  !*halberd*
  !*scythe*
  !*scimitar*
  !*lance*
  !*warhammer*
  !*staff*
  !*mace*
  !*flail*)


(define-piece "shortbow" 35)
(define-piece "longbow" 60)
(define-piece "crossbow" 80)
(define-piece "compound bow" 80)

(define-rule (piece-bow :distribution :weighted)
  (2 !*shortbow*)
  (2 !*longbow*)
  (1 !*crossbow*)
  (1 !*compound-bow*))

(define-rule piece-sling
  !*sling*)


(define-rule (vanilla-weapon :distribution :weighted)
  (2 (metal piece-melee))
  (1 (wood piece-bow))
  (0.5 (skin piece-sling)))

(define-rule (weapon% :distribution :weighted)
  (2 (vanilla-weapon nil))
  (1 (vanilla-weapon enchant-weapon)))

(defun weapon ()
  (destructuring-bind ((material piece) enchantment)
      (weapon%)
    (make-instance 'weapon
      :material material
      :piece piece
      :enchantment enchantment
      :ornament (ornament material))))


(defun weapon-value (weapon)
  (with-weapon (weapon)
    (* (+ (* (_ piece piece-base-value)
             (_ material material-multiplier))
          (if enchantment 100 0)
          (if ornament 10 0))
       (enchantment-multiplier enchantment)
       (if ornament 1.5 1.0))))


(defun vanilla-weapon-description (vanilla-weapon)
  (with-weapon (vanilla-weapon)
    (format nil "~A ~A"
            (_ material material-name)
            (_ piece piece-name))))

(defun weapon-description (weapon)
  (let ((vanilla-description (vanilla-weapon-description weapon))
        (enchantment (weapon-enchantment weapon))
        (ornament (weapon-ornament weapon)))
    (concatenate 'string
                 (if enchantment
                   (enchanted-weapon-description vanilla-description
                                                 enchantment)
                   vanilla-description)
                 (if ornament
                   (format nil ", ~A" ornament)
                   ""))))


;;;; Flavor -------------------------------------------------------------------
(define-string fluid
  "blood"
  "slime")

(define-string sales-pitch
  "Only used once!"
  "Brand new!"
  ("That" fluid "will wash right off...")
  "You won't find a better deal!"
  "This could save your life!"
  "It breaks my heart to part with it..."
  "Its last owner... doesn't need it anymore."
  ("I bought it from" [monster monster-adjective a] role :. ".")
  "Look at the workmanship!"
  "This is gonna go fast!")


;;;; Prices -------------------------------------------------------------------
(defun round-to (n sigfigs)
  (let* ((digits (ceiling (log n 10)))
         (div (expt 10 (max 0 (- digits sigfigs)))))
    (_ n
      (round _ div)
      (* _ div))))

(defun sanitize-price (price)
  (let ((price (round-to price 3)))
    (cond
      ((< price 50) price)
      ((in-range-p 50 price 100) (* 5 (round price 5)))
      ((in-range-p 100 price 1000) (* 10 (round price 10)))
      (t price))))

(defun item-value (item)
  (_ (etypecase item
         (armor (armor-value item))
         (weapon (weapon-value item)))
    (sanitize-price _)
    (format nil "~:D" _)))


(define-string for-the-low-price
  "only"
  "just"
  "yours for the low, low price of"
  "for you, only"
  "on clearance for just"
  "a steal at"
  "on sale for")


;;;; Main ---------------------------------------------------------------------
(define-rule item
  weapon
  armor)

(defun item-description (item)
  (etypecase item
    (armor (armor-description item))
    (weapon (weapon-description item))))

(defun offer ()
  (let ((item (item)))
    $("FOR SALE:" [!item item-description cap] :. "."
      [for-the-low-price cap] [!item item-value] "GP."
      :. #\newline #\newline :.
      sales-pitch)))


(defun dump ()
  (loop :repeat 10 :do
        (terpri)
        (terpri)
        (print '-------------------------------)
        (terpri)
        (princ (offer))))


;;;; API ----------------------------------------------------------------------
(defun random-tweet ()
  (offer))


