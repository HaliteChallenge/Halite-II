(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass command ()
  ((%ship :initarg :ship :reader ship)))

(defclass move-command (command)
  ((%speed :initarg :speed :reader speed :type integer)
   (%angle :initarg :angle :reader angle :type (integer 0 359))))

(defclass dock-command (command)
  ((%planet :initarg :planet :reader planet)))

(defclass undock-command (command)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod write-object ((command move-command) stream)
  (format stream "t ~D ~D~%"
          (ship-id (ship command))
          (speed command)
          (angle command)))

(defmethod write-object ((command dock-command) stream)
  (format stream "d ~D ~D~%"
          (ship-id (ship command))
          (planet-id (planet command))))

(defmethod write-object ((command undock-command) stream)
  (format stream "u ~D~%"
          (ship-id (ship command))))
