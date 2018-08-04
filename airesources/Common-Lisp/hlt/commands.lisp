(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric issue-move-command (ship speed angle))

(defgeneric issue-dock-command (ship planet))

(defgeneric issue-undock-command (ship))

(defgeneric issue-navigate-command
    (ship target &key speed avoid-obstacles max-corrections
                   angular-step ignore-ships ignore-planets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass command ()
  ((%ship :initarg :ship :reader ship)))

(defclass move-command (command)
  ((%move-speed :initarg :speed :reader move-speed :type integer)
   (%move-angle :initarg :angle :reader move-angle :type (integer 0 359))))

(defclass dock-command (command)
  ((%planet :initarg :planet :reader planet)))

(defclass undock-command (command)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod initialize-instance :after ((command command) &rest initargs)
  (declare (ignore initargs))
  (with-accessors ((current-command command)) (ship command)
    (if (null current-command)
        (setf current-command command)
        (error "Multiple commands in one turn for ship ~A." (ship command)))))

(defmethod issue-move-command ((ship ship) (speed float) (angle float))
  (make-instance 'move-command
    :ship ship
    :move-speed (coerce speed 'double-float)
    :move-angle (coerce angle 'double-float)))

(defmethod issue-dock-command ((ship ship) (planet planet))
  (make-instance 'move-command
    :ship ship
    :planet planet))

(defmethod issue-undock-command ((ship ship))
  (make-instance 'move-command
    :ship ship))

(defmethod issue-navigate-command
    ((ship ship) target &key (speed +max-speed+)
                          (avoid-obstacles t)
                          (max-corrections 90)
                          (angular-step 1)
                          (ignore-ships nil)
                          (ignore-planets nil))
  (labels ((path-blocked-p (target)
             (cond ((and ignore-ships ignore-planets)
                    nil)
                   (ignore-ships
                    (null (planets-between ship target)))
                   (ignore-planets
                    (null (ships-between ship target)))
                   (t
                    (null (obstacles-between ship target)))))
           (navigate-to (target max-corrections)
             (let ((distance (distance ship target))
                   (angle (angle-between ship target)))
               (if (or (zerop max-corrections)
                       (path-blocked-p target))
                   (issue-move-command ship (min speed distance) angle)
                   (navigate-to
                    (make-relative-position
                     target
                     (* (cos (+ angle angular-step)) distance)
                     (* (sin (+ angle angular-step)) distance))
                    (1- max-corrections))))))
    (navigate-to target (if avoid-obstacles max-corrections 0))))

