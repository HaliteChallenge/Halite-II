(in-package :halite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Receiving Game Entities

(defvar *input-stream*)

;;; A hash table, mapping from ids to game entities.
(defvar *entity-table*)

(defun next-value ()
  (read *input-stream*))

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

(defun read-list (read-fn)
  (let ((n (next-value)))
    (loop repeat n
          collect (funcall read-fn))))

(defun read-game-map (stream)
  (let ((*entity-table* (make-hash-table))
        (*read-default-float-format* 'double-float)
        (*read-eval* nil)
        (*input-stream* stream))
    (make-instance 'game-map
      :players (read-list #'read-player)
      :planets (read-list #'read-planet)
      :entity-table *entity-table*)))

(defun read-player ()
  (let ((player (make-instance 'player :id (next-value))))
    (reinitialize-instance player :ships (read-list #'read-ship))))

(defun read-ship ()
  (make-instance 'ship
    :id (next-value)
    :x (next-value)
    :y (next-value)
    :health (next-value)
    :x-velocity (next-value)
    :y-velocity (next-value)
    :docking-status
    (case (next-value)
      (0 :undocked)
      (1 :docking)
      (2 :docked)
      (3 :undocking))
    :planet (read-id)
    :progress (next-value)
    :weapon-cooldown (next-value)))

(defun read-planet ()
  (make-instance 'planet
    :id (next-value)
    :x (next-value)
    :y (next-value)
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
  (let ((user-id (read *standard-input*))
        (width (read *standard-input*))
        (height (read *standard-input*)))
    (princ bot-name *standard-output*)
    (let ((game-map (read-game-map *standard-input*)))
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
        (read-game-map *standard-input*)))
