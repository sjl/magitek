(in-package :magitek.robots.rpg-shopkeeper)
(named-readtables:in-readtable :chancery)

;;;; Utils --------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun muffenize (str)
    (symb '* (string-upcase (substitute #\- #\space str)) '*)))


;;;; General ------------------------------------------------------------------
(define-rule (bonus :distribution (:zipf :exponent 1.8))
  1 2 3 4 5)


;;;; Materials ----------------------------------------------------------------
(defclass* material ()
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


(define-string material-picture-wood
  "carved with images of"
  "covered in carvings of"
  "painted with pictures of")

(define-string material-picture-metal
  "engraved with an image of"
  "painted with images of")

(define-string material-picture-textile
  "embroidered with images of")


(defun material-picture (material)
  (ecase (material-kind material)
    (:metal (material-picture-metal))
    (:wood (material-picture-wood))
    (:textile (material-picture-textile))))


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


;;;; Pieces -------------------------------------------------------------------
(defclass* piece ()
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


;;;; Armor --------------------------------------------------------------------
(defclass* armor ()
  (material piece enchantment ornament))

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


(define-rule vanilla-armor
  (metal piece-armor)
  (textile piece-clothing))

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
  (let ((enchantment (armor-enchantment armor)))
    (* (+ (* (-> armor armor-piece piece-base-value)
             (-> armor armor-material material-multiplier))
          (if enchantment 100 0))
       (enchantment-multiplier enchantment)
       (if (armor-ornament armor) 1.5 1.0))))


(defgeneric enchanted-armor-description
  (base enchantment-type enchantment-arguments))

(defmethod enchanted-armor-description
    (base (e (eql :protection)) enchantment-args)
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
  (format nil "~A ~A"
          (-> vanilla-armor armor-material material-name)
          (-> vanilla-armor armor-piece piece-name)))


(defun armor-description (armor)
  (let ((vanilla-description (vanilla-armor-description armor))
        (enchantment (armor-enchantment armor))
        (ornament (armor-ornament armor)))
    (concatenate 'string
                 (if enchantment
                   (enchanted-armor-description vanilla-description
                                                (first enchantment)
                                                (rest enchantment))
                   vanilla-description)
                 (if ornament
                   (format nil ", ~A" ornament)
                   ""))))


; ;;;; Weapons ------------------------------------------------------------------
; (define-rule melee-weapon
;   "dagger"
;   "longsword"
;   "short sword"
;   "hand axe"
;   "battleaxe"
;   "spear"
;   "halberd"
;   "scythe"
;   "scimitar"
;   "lance"
;   "hammer"
;   "staff"
;   "mace"
;   "flail")

; (define-rule (bow-weapon :distribution :weighted)
;   (2 "shortbow")
;   (2 "longbow")
;   (1 "crossbow")
;   (1 "compound bow"))


; (define-rule vanilla-weapon
;   (metal melee-weapon)
;   (wood bow-weapon))

; (define-rule (weapon :distribution :weighted)
;   (1 (vanilla-weapon nil))
;   (1 (vanilla-weapon weapon-enchantment)))


; (defgeneric enchanted-weapon-description
;   (base enchantment-type enchantment-arguments))

; (defmethod enchanted-weapon-description
;     (base (e (eql :slaying)) enchantment-args)
;   (destructuring-bind (monster) enchantment-args
;     (format nil "~A of ~A-slaying" base (monster-singular monster))))

; (defmethod enchanted-weapon-description
;     (base (e (eql :element)) enchantment-args)
;   (destructuring-bind (element) enchantment-args
;     (format nil "~A ~A" (second element) base)))

; (defmethod enchanted-weapon-description
;     (base (e (eql :bonus)) enchantment-args)
;   (destructuring-bind (val) enchantment-args
;     (format nil "+~D ~A" val base)))


; (defun vanilla-weapon-description (vanilla-weapon)
;   (destructuring-bind (material piece) vanilla-weapon
;     (format nil "~A ~A" (material-name material) piece)))


; (defun weapon-description (weapon)
;   (destructuring-bind (vanilla enchant) weapon
;     (let ((vanilla-description (vanilla-weapon-description vanilla)))
;       (if enchant
;         (enchanted-weapon-description vanilla-description
;                                       (first enchant)
;                                       (rest enchant))
;         vanilla-description))))


;;;; Flavor -------------------------------------------------------------------
(define-string sales-pitch
  "Only used once!"
  "Brand new!"
  "The blood will wash right off..."
  ("I bought it from" [monster monster-adjective a] role :. ".")
  "Look at the workmanship!"
  "This is gonna go fast!")


;;;; Main ---------------------------------------------------------------------
(define-rule item
  ; !(weapon-description @weapon)
  armor)

(defun item-description (item)
  (etypecase item
    (armor (armor-description item))))

(defun item-value (item)
  (format nil "~:D"
          (ceiling (etypecase item
                     (armor (armor-value item))))))

(define-string for-the-low-price
  "only"
  "just"
  "yours for the low, low price of"
  "for you, only"
  "a steal at"
  "on sale for")

(defun offer ()
  (let ((item (item)))
    $("FOR SALE:" [!item item-description cap] :. "."
      [for-the-low-price cap] !(item-value item) "GP."
      :. #\newline #\newline :.
      sales-pitch)))


;;;; API ----------------------------------------------------------------------
(defun random-string ()
  (offer))


