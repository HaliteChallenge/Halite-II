(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric make-position (game-map x y))

(defgeneric make-relative-position (position dx dy))

(defgeneric ship-docking-p (ship))

(defgeneric planet-owned-p (planet))

(defgeneric nth-player (n game-map))

(defgeneric angle-between (position-1 position-2))

(defgeneric distance (position-1 position-2))

(defgeneric closest-point-to (source target &key min-distance))

(defgeneric path-intersects-entity-p (source target entity &key fudge))

(defgeneric nearby-entities (position))

(defgeneric nearby-entities-by-distance (position))

(defgeneric obstacles-between (source target))

(defgeneric ships-between (souce target))

(defgeneric planets-between (source target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass id-mixin ()
  ((%id :initarg :id :reader id)))

(defclass game-map-mixin ()
  ((%game-map :initarg :game-map :reader game-map)))

(defclass pos (game-map-mixin)
  ((%x :initarg :pos-x :reader pos-x :type double-float)
   (%y :initarg :pos-y :reader pos-y :type double-float)))

(defclass command-mixin ()
  ((%command :initform nil :accessor command)))

(defclass player (id-mixin game-map-mixin)
  ((%ships :initarg :ships :reader ships)))

(defclass entity (id-mixin pos)
  ((%radius :initarg :radius :reader radius)
   (%health :initarg :health :reader health)
   (%owner :initarg :owner :reader owner)))

(defclass planet (entity)
  ((%production :initarg :production :reader production)
   (%resources :initarg :resources :reader resources)
   (%docking-spots :initarg :docking-spots :reader docking-spots)
   (%docked-ships :initarg :docked-ships :reader docked-ships)))

(defclass ship (entity command-mixin)
  ((%vel-x :initarg :vel-x :reader vel-x :type double-float)
   (%vel-y :initarg :vel-y :reader vel-y :type double-float)
   (%docking-status :initarg :docking-status :reader docking-status)
   (%planet :initarg :planet :reader planet :initform nil)
   (%progress :initarg :progress :reader progress)
   (%weapon-cooldown :initarg :weapon-cooldown :reader weapon-cooldown)))

(defclass game-map ()
  ((%players :initarg :players :reader players)
   (%planets :initarg :planets :reader planets)))

(defclass game ()
  ((%width :initarg :width :reader width)
   (%height :initarg :height :reader height)
   (%user-id :initarg :user-id :reader user-id)
   (%bot-name :initarg :bot-name :reader bot-name)
   (%initial-map :initarg :initial-map :reader initial-map)
   (%current-map :initarg :current-map :accessor current-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods and Functions

(defmethod make-position ((game-map game-map) (x float) (y float))
  (make-instance 'position
    :game-map game-map
    :pos-x (coerce x 'double-float)
    :pos-y (coerce y 'double-float)))

(defmethod make-relative-position ((pos pos) (dx float) (dy float))
  (make-instance 'position
    :game-map (game-map pos)
    :pos-x (+ (pos-x pos) (coerce dx 'double-float))
    :pos-y (+ (pos-y pos) (coerce dy 'double-float))))

(defmethod ship-docking-p (ship)
  (eq (docking-status ship) :docking))

(defmethod planet-owned-p (planet)
  (not (null (owner planet))))

(defmethod nth-player (id game-map)
  (find id (players game-map) :key #'id))

(defmethod distance ((a pos) (b pos))
  (sqrt (+ (expt (- (pos-x a) (pos-x b)) 2)
           (expt (- (pos-y a) (pos-y b)) 2))))

(defmethod angle-between ((source pos) (target pos))
  (atan (- (pos-y target) (pos-y source))
        (- (pos-x target) (pos-x source))))

(defmethod closest-point-to ((source pos) (target pos) &key (min-distance 3))
  (let ((angle (angle-between source target))
        (radius (+ (radius target) min-distance)))
    (values (+ (pos-x target) (* radius (cos angle)))
            (+ (pos-y target) (* radius (sin angle))))))

(defmethod path-intersects-entity-p ((source pos) (target pos) (entity entity)
                                     &key (fudge +ship-radius+))
  (let ((sx (pos-x source)) (sy (pos-y source))
        (tx (pos-x target)) (ty (pos-y target))
        (ex (pos-x entity)) (ey (pos-y entity)))
    (let ((dx (- tx sx))
          (dy (- ty sy)))
      (let ((a (+ (* dx dx) (* dy dy)))
            (b (* -2 (- (+ (* sx sx) (* sy sy)
                           (* tx ex) (* ty ey))
                        (* sx tx) (* sx ex)
                        (* sy ty) (* sy ey)))))
        (if (zerop a)
            (<= (distance source entity) (+ (radius entity) fudge))
            (let ((d (if (zerop a) 0 (max 0.0d0 (min (/ (- b) (* 2 a)) 1.0d0)))))
              (<= (distance (make-relative-position source (* dx d) (* dy d)) entity)
                  (+ (radius entity) fudge))))))))

(defmethod obstacles-between ((source pos) (target pos))
  (append (ships-between source target)
          (planets-between source target)))

(defmethod ships-between ((source pos) (target pos))
  (loop for player in (players (game-map source)) do
    (loop for ship in (ships player)
          unless (eq ship source)
            unless (eq ship target)
              when (path-intersects-entity-p source target ship)
                collect ship)))

(defmethod planets-between ((source pos) (target pos))
  (loop for planet in (planets (game-map source))
        unless (eq planet source)
          unless (eq planet target)
            when (path-intersects-entity-p source target planet)
              collect planet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing of Game Entities

(defmethod print-object ((entity entity) stream)
  (print-unreadable-object (entity stream :type t)
    (format stream ":ID ~S :POS-X ~S :POS-Y ~S :HEALTH ~S :OWNER ~S"
            (id entity) (pos-x entity) (pos-y entity) (health entity) (id (owner entity)))))
