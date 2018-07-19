(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric angle-between (entity-1 entity-2))

(defgeneric distance (entity-1 entity-2))

(defgeneric closest-point-to (entity target &key min-distance))

(defgeneric nearby-entities (entity))

(defgeneric nearby-entities-by-distance (entity))

(defgeneric write-object (object stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass id-mixin ()
  ((%id :initarg :id :reader id)))

(defclass command-mixin ()
  ((%command :initarg :command :reader command)))

(defclass player (id-mixin)
  ((%ships :initarg :ships :reader ships)))

(defclass entity (id-mixin)
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y)
   (%radius :initarg :radius :reader radius)
   (%health :initarg :health :reader health)
   (%owner :initarg :owner :reader owner)))

(defclass planet (entity)
  ((%production :initarg :production :reader production)
   (%resources :initarg :resources :reader resources)
   (%docking-spots :initarg :docking-spots :reader docking-spots)
   (%docked-ships :initarg :docked-ships :reader docked-ships)))

(defclass ship (entity command-mixin)
  ((%x-vecocity :initarg :x-velocity :reader x-velocity)
   (%y-vecocity :initarg :y-velocity :reader y-velocity)
   (%status :initarg :status :reader status :type docking-status)
   (%planet :initarg :planet :reader planet :initform nil)
   (%progress :initarg :progress :reader progress)
   (%weapon-cooldown :initarg :weapon-cooldown :reader weapon-cooldown)))

(defclass game-map ()
  ((%players :initarg :players :reader players)
   (%planets :initarg :planets :reader planets)
   (%entity-table :initarg :entity-table :reader entity-table)))

(defclass game ()
  ((%user-id :initarg :user-id :reader user-id)
   (%bot-name :initarg :bot-name :reader bot-name)
   (%initial-map :initarg :initial-map :reader initial-map)
   (%current-map :initarg :current-map :accessor current-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod distance ((a entity) (b entity))
  (sqrt (+ (expt (- (x a) (x b)) 2)
           (expt (- (y a) (y b)) 2))))

(defmethod angle-between ((entity entity) (target entity))
  (atan (- (y target) (y entity))
        (- (x target) (x entity))))

(defmethod closest-point-to ((entity entity) (target entity) &key (min-distance 3))
  (let ((angle (angle-between entity target))
        (radius (+ (radius target) min-distance)))
    (values (+ (x target) (* radius (cos angle)))
            (+ (y target) (* radius (sin angle))))))
