(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Receiving Game Entities

;;; A hash table, mapping from ids to game entities.
(defvar *entity-table*)

(defun next-value ()
  (read *standard-input*))

(defmethod initialize-instance :after ((instance id-mixin) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (id instance) *entity-table*)
        instance))

(defun read-id ()
  (let ((id (next-value)))
    (if (zerop id)
        :unknown-entity
        (or (gethash id *entity-table*)
            (error "Invalid id: ~D." id)))))

(defun read-list (read-fn &rest args)
  (let ((n (next-value)))
    (loop repeat n collect (apply read-fn args))))

(defun read-game-map ()
  (let ((*entity-table* (make-hash-table))
        (*read-default-float-format* 'double-float)
        (*read-eval* nil)
        (game-map (make-instance 'game-map)))
    (reinitialize-instance game-map
      :players (read-list #'read-player game-map)
      :planets (read-list #'read-planet game-map))))

(defun read-player (game-map)
  (let ((player (make-instance 'player
                  :id (next-value)
                  :game-map game-map)))
    (reinitialize-instance player
      :ships (read-list #'read-ship game-map player))))

(defun read-ship (game-map owner)
  (make-instance 'ship
    :game-map game-map
    :owner owner
    :id (next-value)
    :pos-x (next-value)
    :pos-y (next-value)
    :health (next-value)
    :vel-x (next-value)
    :vel-y (next-value)
    :docking-status
    (case (next-value)
      (0 :undocked)
      (1 :docking)
      (2 :docked)
      (3 :undocking))
    :planet (read-id)
    :progress (next-value)
    :weapon-cooldown (next-value)))

(defun read-planet (game-map)
  (make-instance 'planet
    :game-map game-map
    :id (next-value)
    :pos-x (next-value)
    :pos-y (next-value)
    :health (next-value)
    :radius (next-value)
    :docking-spots (next-value)
    :production (next-value)
    :resources (next-value)
    :owner
    (let ((owner-p (zerop (next-value)))
          (owner-id (next-value)))
      (and owner-p (gethash owner-id *entity-table*)))
    :docked-ships
    (loop repeat (next-value)
          collect (read-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; High-level Interface

(defun make-game (&key bot-name)
  (check-type bot-name string)
  (let ((user-id (next-value))
    (width (next-value))
    (height (next-value)))
    (princ bot-name *standard-output*)
    (let ((game-map (read-game-map)))
      (make-instance 'game
        :user-id user-id
        :bot-name bot-name
        :width width
        :height height
        :initial-map game-map
        :current-map game-map))))

(defun finalize-turn (game)
  ;; Send all commands.
  (let ((player (find (user-id game)
                      (players (current-map game))
                      :key #'id)))
    (loop for ship in (ships player) do
      (send-command (command ship) *standard-output*))
    (terpri *standard-output*)
    (finish-output *standard-output*))
  ;; Read in the new map
  (setf (current-map game)
        (read-game-map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sending Game Entities

(defgeneric send-command (command stream))

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
