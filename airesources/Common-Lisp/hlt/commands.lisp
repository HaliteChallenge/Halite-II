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
  ((%move-speed :initarg :speed :reader move-speed)
   (%move-angle :initarg :angle :reader move-angle)))

(defclass dock-command (command)
  ((%planet :initarg :planet :reader planet)))

(defclass undock-command (command)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod initialize-instance :after ((command command) &rest initargs)
  (declare (ignore initargs))
  (setf (command (ship command)) command))

(defmethod issue-move-command ((ship ship) (speed number) (angle number))
  (when (minusp speed)
    (incf angle pi)
    (setf speed (abs speed)))
  (make-instance 'move-command
    :ship ship
    :speed (min speed +max-speed+)
    :angle (mod angle (* 2 pi))))

(defmethod issue-dock-command ((ship ship) (planet planet))
  (when (<= (distance ship planet)
            (+ (radius planet) (radius ship) +dock-radius+))
    (make-instance 'dock-command
      :ship ship
      :planet planet)))

(defmethod issue-undock-command ((ship ship))
  (when (ship-docking-p ship)
    (make-instance 'undock-command
      :ship ship)))

;;; This path finding function uses a very naive algorithm.  If you want to
;;; turn this starter kit into a competitive AI, you should definitely
;;; replace this code with something more sophisticated.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printing of Commands

(defmethod print-object ((command move-command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream ":SHIP-ID ~S :SPEED ~S :ANGLE ~S"
            (id (ship command)) (move-speed command) (move-angle command))))

(defmethod print-object ((command dock-command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream ":SHIP-ID ~S :PLANET-ID ~S"
            (id (ship command)) (id (planet command)))))

(defmethod print-object ((command undock-command) stream)
  (print-unreadable-object (command stream :type t)
    (format stream ":SHIP-ID ~S"
            (id (ship command)))))
