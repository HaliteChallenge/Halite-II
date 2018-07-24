(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric send-command (command stream))

(defgeneric issue-move-command (ship speed angle))

(defgeneric issue-dock-command (ship planet))

(defgeneric issue-undock-command (ship))

(defgeneric issue-navigate-command (ship &key destination speed ignore-ships))

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

(defmethod issue-move-command ((ship ship) speed angle)
  (make-instance 'move-command
    :ship ship
    :move-speed speed
    :move-angle angle))

(defmethod issue-dock-command ((ship ship) (planet planet))
  (make-instance 'move-command
    :ship ship
    :planet planet))

(defmethod issue-undock-command ((ship ship))
  (make-instance 'move-command
    :ship ship))

(defmethod issue-navigate-command ((ship ship) &key destination speed ignore-ships)
  (values))

(defmethod send-command ((command null) stream)
  (declare (ignore command stream))
  (values))

(defmethod send-command ((command move-command) stream)
  (format stream "t ~D ~D ~D~%"
          (id (ship command))
          (move-speed command)
          (move-angle command)))

(defmethod send-command ((command dock-command) stream)
  (format stream "d ~D ~D~%"
          (id (ship command))
          (id (planet command))))

(defmethod send-command ((command undock-command) stream)
  (format stream "u ~D~%"
          (id (ship command))))
