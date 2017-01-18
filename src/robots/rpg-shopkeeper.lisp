(in-package :magitek.robots.rpg-shopkeeper)
(named-readtables:in-readtable :chancery)

;;;; General ------------------------------------------------------------------
(define-rule (bonus :distribution (:zipf :exponent 1.8))
  1 2 3 4 5)


;;;; Materials ----------------------------------------------------------------
(defclass* material ()
  (name multiplier))

(defun make-material (name multiplier)
  (make-instance 'material :name name :multiplier multiplier))

(defmethod print-object ((o material) s)
  (print-unreadable-object (o s :type t)
    (princ (material-name o) s)))

(defmacro define-material (name multiplier)
  `(defparameter ,(symb '* (string-upcase (substitute #\- #\space name)) '*)
     (make-material ,name ,multiplier)))


(define-material "iron" 1.0)
(define-material "steel" 1.5)
(define-material "silver" 2.0)
(define-material "meteoric iron" 4.0)
(define-material "mithril" 10.0)
(define-material "adamantine" 11.0)


(define-material "cloth" 1.0)
(define-material "leather" 1.5)
(define-material "silk" 2.0)
(define-material "spider silk" 6.0)


(define-material "pine" 1.0) ; http://www.wood-database.com/wood-articles/bow-woods/
(define-material "poplar" 1.2)
(define-material "walnut" 1.3)
(define-material "maple" 2.0)
(define-material "oak" 3.0)
(define-material "yew" 5.0)
(define-material "rosewood" 10.0)


(define-rule (metal :distribution :zipf)
  !*iron*
  !*steel*
  !*silver*
  !*meteoric-iron*
  !*mithril*
  !*adamantine*)

(define-rule (textile :distribution :weighted)
  (1.00 !*cloth*)
  (1.00 !*leather*)
  (0.80 !*silk*)
  (0.01 !*spider-silk*))

(define-rule (wood :distribution (:zipf :exponent 0.8))
  !*pine*
  !*poplar*
  !*walnut*
  !*maple*
  !*oak*
  !*yew*
  !*rosewood*)


;;;; Monsters -----------------------------------------------------------------
(defclass* monster ()
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
  `(defparameter ,(symb '* (string-upcase singular) '*)
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


;;;; Roles --------------------------------------------------------------------
(define-rule role
  "cleric"
  "warrior"
  "wizard"
  "rogue")


;;;; Elements -----------------------------------------------------------------
(define-rule element
  ("ice"         "icy")
  ("fire"        "flaming")
  ("electricity" "electrified")
  ("poison"      "venomous"))


;;;; Armor --------------------------------------------------------------------
(define-rule armor-piece
  "scale mail"
  "ring mail"
  "chain mail"
  "plate mail")

(define-rule clothing-piece
  "robe"
  "shirt"
  "pants"
  "dress"
  "skirt")


(define-rule armor-enchantment
  (:protection-from monster)
  (:resistance element)
  (:bonus bonus))

(define-rule vanilla-armor
  (metal armor-piece)
  (textile clothing-piece))

(define-rule (armor :distribution :weighted)
  (2 (vanilla-armor nil))
  (1 (vanilla-armor armor-enchantment)))


(defgeneric enchanted-armor-description
  (base enchantment-type enchantment-arguments))

(defmethod enchanted-armor-description
    (base (e (eql :protection-from)) enchantment-args)
  (destructuring-bind (monster) enchantment-args
    (format nil "~A of protection from ~A" base (monster-plural monster))))

(defmethod enchanted-armor-description
    (base (e (eql :resistance)) enchantment-args)
  (destructuring-bind ((noun adjective)) enchantment-args
    (declare (ignore adjective))
    (format nil "~A of ~A resistance" base noun)))

(defmethod enchanted-armor-description
    (base (e (eql :bonus)) enchantment-args)
  (destructuring-bind (val) enchantment-args
    (format nil "+~D ~A" val base)))


(defun vanilla-armor-description (vanilla-armor)
  (destructuring-bind (material piece) vanilla-armor
    (format nil "~A ~A" (material-name material) piece)))


(defun armor-description (armor)
  (destructuring-bind (vanilla enchant) armor
    (let ((vanilla-description (vanilla-armor-description vanilla)))
      (if enchant
        (enchanted-armor-description vanilla-description
                                     (first enchant)
                                     (rest enchant))
        vanilla-description))))


;;;; Weapons ------------------------------------------------------------------
(define-rule melee-weapon
  "dagger"
  "longsword"
  "short sword"
  "hand axe"
  "battleaxe"
  "spear"
  "halberd"
  "scythe"
  "scimitar"
  "lance"
  "hammer"
  "staff"
  "mace"
  "flail")

(define-rule (bow-weapon :distribution :weighted)
  (2 "shortbow")
  (2 "longbow")
  (1 "crossbow")
  (1 "compound bow"))


(define-rule weapon-enchantment
  (:slaying monster)
  (:element element)
  (:bonus bonus))


(define-rule vanilla-weapon
  (metal melee-weapon)
  (wood bow-weapon))

(define-rule (weapon :distribution :weighted)
  (1 (vanilla-weapon nil))
  (1 (vanilla-weapon weapon-enchantment)))


(defgeneric enchanted-weapon-description
  (base enchantment-type enchantment-arguments))

(defmethod enchanted-weapon-description
    (base (e (eql :slaying)) enchantment-args)
  (destructuring-bind (monster) enchantment-args
    (format nil "~A of ~A-slaying" base (monster-singular monster))))

(defmethod enchanted-weapon-description
    (base (e (eql :element)) enchantment-args)
  (destructuring-bind (element) enchantment-args
    (format nil "~A ~A" (second element) base)))

(defmethod enchanted-weapon-description
    (base (e (eql :bonus)) enchantment-args)
  (destructuring-bind (val) enchantment-args
    (format nil "+~D ~A" val base)))


(defun vanilla-weapon-description (vanilla-weapon)
  (destructuring-bind (material piece) vanilla-weapon
    (format nil "~A ~A" (material-name material) piece)))


(defun weapon-description (weapon)
  (destructuring-bind (vanilla enchant) weapon
    (let ((vanilla-description (vanilla-weapon-description vanilla)))
      (if enchant
        (enchanted-weapon-description vanilla-description
                                      (first enchant)
                                      (rest enchant))
        vanilla-description))))


;;;; Flavor -------------------------------------------------------------------
(define-string scene
  ("images of" [monster monster-plural] "fighting" [monster monster-plural])
  ("a picture of a famous" [monster monster-singular] role))

(define-string picture
  "painted with"
  "adorned with")

(define-string flavor
  (picture scene))

(define-string sales-pitch
  "Only used once!"
  "Brand new!"
  ("I bought it from" [monster monster-adjective a] role :. ".")
  "Look at the workmanship!"
  "This is gonna go fast!")


;;;; Main ---------------------------------------------------------------------
(define-rule base-item
  !(weapon-description @weapon)
  !(armor-description @armor))

(define-string (item :distribution :weighted)
  (2 base-item)
  (1 (base-item :. "," flavor)))

(define-string offer
  ("FOR SALE:" [item cap]
   :. #\newline #\newline :.
   sales-pitch))


;;;; API ----------------------------------------------------------------------
(defun random-string ()
  (offer))


