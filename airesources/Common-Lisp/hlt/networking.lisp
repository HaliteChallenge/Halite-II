(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric send-bot-name (game bot-name))

(defgeneric finalize-turn (game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Receiving Game Entities

;;; Hash tables mapping from ids to game entities.

(defvar *player-table*)

(defvar *ship-table*)

(defvar *planet-table*)


(defun next-value ()
  (read *standard-input*))

(defmethod initialize-instance :after ((instance player) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (id instance) *player-table*) instance))

(defmethod initialize-instance :after ((instance ship) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (id instance) *ship-table*) instance))

(defmethod initialize-instance :after ((instance planet) &rest initargs)
  (declare (ignore initargs))
  (setf (gethash (id instance) *planet-table*) instance))

(defun read-by-id (table)
  (let ((id (next-value)))
    (gethash id table)))

(defun read-ship-by-id ()
  (read-by-id *ship-table*))

(defun read-planet-by-id ()
  (read-by-id *planet-table*))

(defun read-list (read-fn &rest args)
  (let ((n (next-value)))
    (loop repeat n collect (apply read-fn args))))

(defun read-game-map ()
  (let ((*player-table* (make-hash-table))
        (*ship-table* (make-hash-table))
        (*planet-table* (make-hash-table))
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
    :planet (read-planet-by-id)
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
    (let ((owner-p (plusp (next-value)))
          (owner-id (next-value)))
      (and owner-p (gethash owner-id *player-table*)))
    :docked-ships
    (loop repeat (next-value)
          collect (read-ship-by-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; High-level Interface

(defun make-game ()
  (let ((user-id (next-value))
    (width (next-value))
    (height (next-value)))
    (let ((game-map (read-game-map)))
      (make-instance 'game
        :user-id user-id
        :width width
        :height height
        :initial-map game-map
        :current-map game-map))))

(defmethod send-bot-name ((game game) (bot-name string))
  (format *standard-output* "~S~%" bot-name)
  (finish-output *standard-output*))

(defmethod finalize-turn ((game game))
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
  (format stream "t ~D ~D ~D "
          (id (ship command))
          (round (move-speed command))
          (round (* (move-angle command) 180.0d0 (/ pi)))))

(defmethod send-command ((command dock-command) stream)
  (format stream "d ~D ~D "
          (id (ship command))
          (id (planet command))))

(defmethod send-command ((command undock-command) stream)
  (format stream "u ~D "
          (id (ship command))))
