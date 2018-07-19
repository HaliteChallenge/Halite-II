;;;; Welcome to your first Halite-II bot in Common Lisp!
;;;;
;;;; The bot's name is Settler.  It executes a very simple algorithm:
;;;;
;;;; 1. Initialize the game
;;;;
;;;; 2. If a ship is not docked and there are unowned planets.
;;;; 2.a Try to dock the planet if it is close enough.
;;;; 2.b If not, move towards that planet.

(defpackage :mybot
  (:use :common-lisp))

(in-package :mybot)

(defvar *game*)

(defun log (control-string &rest args)
  (apply #'format *trace-output* control-string args)
  (finish-output *trace-output*))

(defun mybot ()
  ;; Initialize the game.
  (let ((*game* (hlt:make-game :bot-name "Settler")))
    ;; Optional: Describe what your bot is doing.
    (log "Settler bot is now up and running!")
    (loop
      (let ((map (update-map *game*)))
        ;; Search all of your ships.
        (loop for ship in (all-my-ships map)
              ;; Skip ships that are currently docking.
              unless (ship-docking-p ship) do
                ;; Search all planets.
                (loop for planet in (all-planets map)
                      ;; Skip planets that are already owned.
                      unless (planet-owned-p planet) do
                        ;; Either try to dock, or try to move closer to the
                        ;; planet.
                        (or (issue-dock-command ship planet)
                            (issue-navigate-command
                             ship
                             :destination (closest-point-to ship planet)
                             :speed (floor hlt:+max-speed+ 2)
                             :ignore-ships t))))
        ;; Send all the issued commands to the game server.
        (finalize-turn *game*)))))
