(defpackage #:option-9
  (:use #:cl)
  (:export #:option-9))

(in-package #:option-9)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3))) ;; After evaluating it it returns nil. So, I may need to look into it.

(defvar *game*)
(defvar *all-entities*)
(defvar *id*)

(defun new-id ()
  (let ((id *id*))
    (incf *id*)
    id))

(defun euclidean-distance (fx fy tx ty)
  (let ((factor-1 (- tx fx))
	(factor-2 (- ty fy)))
    (let ((ndist (+ (* factor-1 factor-1)
		    (* factor-2 factor-2))))
      (valurs (sqrt ndist) ndist))))


;; This is the entity class which holds the information about various variables  which are going to be used in the game.
;; initarg stands for initial value
;; initform stands for default value
;; accessor can be used as the getter and the setter function as well. Its argument is a name that will become a generic function.
(defclass entity ()
  ((%id :initarg :id
	:initform (new-id)
	:accessor id)
   (%game-context :initarg :game-context
		  :initform nil
		  :accessor game-context)
   (%max-hit-points :initarg :max-hit-points
		    :initform 10
		    :accessor max-hit-points)
   (%hit-points :initarg :hit-points
		:initform 10
		:accessor hit-points)
   (%damage-points :initarg :damage-points
		   :initform 10
		   :accessor damage-points)
   (%points :initarg :points
	    :initform 0
	    :accessor points)
   (%status :initarg :status
	    :initform :alive
	    :accessor status)
   (%charge :initarg :charge
	    :initform -1
	    :accessor charge)
   (%initial-sparks :initarg :initial-sparks
		    :initform 10
		    :accessor initial-sparks)
   (%additional-sparks :initarg :additional-sparks
		       :initform 50
		       :accessor additional-sparks)
   (%auto-finish-construction :initargs :auto-finish-construction
			      :initform t
			      :accessor auto-finish-construction))
  (:documentation "The Entity Class"))


;; This class holds the information about how long something will last in the game.
;; If the entity is epehemeral i.e. lasts for a very short period of time, then it will have no impact in the game.
(defclass ephermal ()
  ((%ttl :initarg :ttl
	 :initform nil
	 :accessor ttl)
   (%ttl-max :initarg :ttl-max
	     :initform nil
	     :accessor ttl-max))
  (:documentation
   "The Ephermal class. Used for things which need a temporal time limit"))

;; The following class is used for physical locations and their directions of movement.
(defclass location ()
  ((%x :initarg :x
       :initform 0
       :accessor x)
   (%y :initarg :y
       :initform 0
       :accessor y)
   (%dx :initarg :dx
	:initform 0
	:accessor :dx)
   (%dy :initarg :dy
	:initform 0
	:accessor :dy))
  (:documentation
   "The Location class. Used to hold the current position and direction vector at that position"))

;; This class holds the gemoetry related information of the entity in the game.
;; The primitives slot consists of lists opengl primitives which contains lists of vertex/color pairs.
;; If we want to have animated frames or textures, this is where they would exist.
(defclass shape ()
  ((%primitive :initarg :primitives
	       :initform  nil
	       :accessor primitives))
  (:documentation "The shape class"))

;; Each entity in a game must have a physical and temporal connection to the game. This class combines those concepts into one idea.
;; FYI, the arguments after the class names represents the classes from which the current class is inheriting from.
(defclass frame (ephermal location)
  ()
  (:documentation "The frame class"))

;; This class just inherits from the direct superclasses and provide one interface to the threee distinct pieces of functionality.
(defclass drawable (entity frame shape)
  ()
  (:documentation "The drawable class"))

;; This class represents the information needed to determine two objects are colliding or not.
;; It uses the centroids of the objects to check the collision.
;; This means that their edges may actually collid and yet the collison may not be detected.
(defclass collidable (drawable)
  ((%radius :initarg :radius
	    :initform 0
	    :accessor radius))
  (:documentation "The Collidable class"))

;; Digits are just the objects which cant really collide and are just drawable. They are just objects which are a vector font for the numbers zero to nine.
(defclass digit (drawable)
  ()
  (:documentation "The digit class"))

;; This class represents how the AI is implemented.
;; When the %until-next-action becomes 0, the game entity may think about what it wants to do during the current game state.
;; Simply speaking, it only decides when/how to have an enemy fire a shot at the player. Simple enough.
(defclass brain (collidable)
  ((%until-next-action :initarg :until-next-action
		       :initform 0
		       :accessor until-next-action))
  (:documentation "The brain class"))

;; This class represents the powerups required by the player. For example, different shot types, different shield types and so on.
;; There are different shot types and shield types but the reason why there is only one powerup type is that they are all identical.
(defclass powerup (brain)
  ((%main-gun :initarg :main-gun
	      :initform nil
	      :accessor powerup-main-gun)
   (%passive-gun :initarg :passive-gun
		 :initform nil
		 :accessor powerup-passive-gun)
   (&main-shield :initarg :main-shield
		 :initform nil
		 :accessor powerup-main-shield)
   (%health-level :initarg :health-level
		  :initform 0
		  :accessor powerup-health-level))
  (:documentation "The powerup class"))


;; This class represents the ship. It has a main-gun, a passive gun and a shield.
(defclass ship (brain)
  ((%main-gun :initarg :main-gun
	      :initform nil
	      :accessor ship-main-gun)
   (%passive-gun :initarg :passive-gun
		 :initform nil
		 :accessor ship-main-shield)
   (%main-shield :initarg :main-shield
		 :initform nil
		 :accessor ship-main-shield))
  (:documentation "The ship class"))

;; This class represents the player.
;; We dont use the ship class itself to create the player ship but instead, we inherit the ship class into the player class.
;; This is because of the fact that in this way, we can figure out what happens when the player ship and the enemy ship collide.
(defclass player (ship)
  ()
  (:documentation "The player class"))


;; The following classes determine the enemy classes enemy, enemy-1, enemy-2, enemy-3
;; These classes exist to allow the multi-methods to work effectively.
(defclass enemy (ship)
  ()
  (:documentation "The enemy base class"))

(defclass enemy-1 (enemy )
  ()
  (:documentation "The enemy 1 class"))

(defclass enemy-2 (enemy)
  ()
  (:documentation "The enemy-2 class"))

(defclass enemy-3 (enemy)
  ()
  (:documentation "The enemy-3 class"))

;; The weapon class is the one from which all the weapons classes are generated.
(defclass weapon (brain)
  ()
  (:documentation "The weapons class"))

;; The following classes represents the different shots in the game.
;; These classes exist to allow multi-methods to work effectively.
(defclass shot (weapon)
  ()
  (:documentation "the shot class"))

(defclass simple-shot (shot)
  ()
  (:documentation "the simple shot class"))

(defclass hardnose-shot (shot)
  ()
  (:documentation "The shots which are not destroyed by bullets"))

(defclass super-shot (shot)
  ()
  (:documentation "The shots which are not destroyed by ships"))

;; The following represents the mine objects
;; Mines can be dropped by enemies when they die.
;; Proximity mines dont move but they have a large collision radius and can be destriyed by Tesla fields.
;; Field mines are immune to the Tesla Field.
(defclass mine (weapon)
  ()
  (:documentation "The base mine class"))

(defclass proximity-mine (mine)
  ()
  (:documentation "The proximity mine class"))

(defclass field-mine (mine)
  ()
  (:documentation "The field mine class"))

;; The following classes are for the shields.
;; shot-shield protects the ship only from the shots, but a ship shield protects from the shots and the shield as well.
(defclass shield (brain)
  ((%shots-absorbed :initarg :shots-absorbed
		    :initform 5
		    :accessor shots-absorbed))
  (:documentation "The Shield base class "))

(defclass shot-shield (shield)
  ()
  (:documentation "The Shot shield classs"))

(defclass ship-shield (shield)
  ()
  (:documentation "The ship shield class"))


(defclass fieldpath ()
  ;; How many steps the path went before it actually hit something or
  ;; reached the end of its range. This is in world space.
  ((%steps :initarg :steps
	   :initform  0
	   :accessor steps)
   ;; The vector containing the location coordinates of each step with
   ;; element 0 being the start of the path.
   (%path :initarg :path
	  :initform nil
	  :accessor path))
  (:documentation " the field path class"))

;; PathContacts are used as values in a hash table where the keys are either the entity id or the key :no-collision.
;; they record how many and which field paths contacted an entity.
;; For the paths with key :no-collision, it just means that the path existws, it just didnt hit anything.

(defclass pathcontact ()
  ((%number-of-contacts :initarg :number-of-contacts
			:initform 0
			:accessor number-of-contacts)
   (%path-ids :initarg :contacts
	      :initform nil
	      :accessor path-ids))
  (:documentation "The path contact class. This is stored on a per entity basis and records the field path-ids that touch that particular entitiy."))

(defclass field ()
  ;; This range is described in the number of steps I should follow the field line trace
  ((%range :initarg :range
	   :initform  1
	   :accessor range)
   ;; Num paths are how many even distributed paths should be followed from around the field generating object.
   (%num-paths :initarg :num-paths
	   :initform 1
	   :accessor num-paths)
   (paths :initarg :traces
	  :initform nil
	  :accessor paths)
   ;; A hash table of pathcontact classes keyed by the entity id the trace touches, or "no-id" if it doesnt touch.
   (%entity-contacts :initarg :contacts
		     :initform (make-hash-table :test #'equal)
		     :accessor entity-contacts))
  (:documentation "The field class"))

;; Reader function is also called the setter function.
(defclass tesla-field (field weapon)
   ;; This is a quantized range of power for the tesla-field
  ((%power-range :initarg :power-range
		 :initform 1
		 :reader power-range)
   (%power-lines :initarg :power-lines
		 :initform  1
		 :reader power-lines))
  (:documentation "The tesla field class"))


;; Each thing in the game is kept in its particular list. This makes it easy to perform collision detection only as necessary.
(defclass game ()
  ((%players :initarg :players
	     :initform nil
	     :accessor players)
   (%player-shots :initarg :player-shots
		  :initform nil
		  :accessor player-shots)
   (%enemy-mines :initarg :mines
		 :initform  nil
		 :accessor enemy-mines)
   (%enemies :initarg :enemies
	     :initform  nil
	     :accessor enemies)
   (%enemy-shots :initarg :enemy-shots
		 :initform nil
		 :accessor enemy-shots)
   (%sparks :initarg :sparks
	    :initform  nil
	    :accessor sparks)
   (%power-ups :initarg :power-ups
	       :initform nil
	       :accessor power-ups)
   (%score :initarg :score
	   :initform 0
	   :accessor score)
   (%score-board :initarg :score-board
		 :initform  nil
		 :accessor score-board)
   (%highscore :initarg :highscore
	       :initform  0
	       :accessor highscore)
   (%highscore-board :initarg :highscore-board
		     :initform  60
		     :accessor enemy-spawn-timer)
   (%paused :initarg :paused
	    :initform nil
	    :accessor paused))
  (:documentation "The Game class"))

