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
  (:use :common-lisp)
  (:export #:mybot))

(in-package :mybot)

(defvar *game*)

(defvar *logfile*)

(defun open-logfile (user-id bot-name)
  (open (multiple-value-bind (second minute hour day month year)
            (get-decoded-time)
          (format nil "~D-~D-~DT~2,'0D:~2,'0D:~2,'0D-~D-~A.log"
                  year month day hour minute second user-id bot-name))
        :direction :output
        :element-type 'extended-char
        :external-format :utf-8
        :if-exists :supersede))

(defun mybot ()
  ;; Initialize the game.
  (let* ((bot-name "Settler")
         (*game* (hlt:make-game))
         ;; The streams *standard-input* and *standard-output* are used to
         ;; communicate with the Halite application and therefore cannot be
         ;; used to print debug output.  Instead, we open a log file and
         ;; bind it to the special variable *logfile*, such that debugging
         ;; commands can write there instead.
         (*logfile* (open-logfile (hlt:user-id *game*) bot-name)))
    ;; Optional: Describe what your bot is doing.
    (format *logfile* "Bot is now up and running!~%")
    (hlt:send-bot-name *game* bot-name)
    (loop
      (format *logfile* "~&~%== Next Timestep ==~%")
      ;; Ensure that all logging is actually written to the file.
      (finish-output *logfile*)
      ;; Determine the current map and active player.
      (let* ((map (hlt:current-map *game*))
             (active-player (hlt:nth-player (hlt:user-id *game*) map)))
        ;; Search all of your ships.
        (loop for ship in (hlt:ships active-player)
              ;; Skip ships that are currently docking.
              unless (hlt:ship-docking-p ship) do
                ;; Search all planets.
                (loop for planet in (hlt:planets map)
                      ;; Skip planets that are already owned.
                      unless (hlt:planet-owned-p planet) do
                        ;; Either try to dock, or try to move closer to the
                        ;; planet.
                        (or (hlt:issue-dock-command ship planet)
                            (hlt:issue-navigate-command
                             ship
                             (hlt:closest-position ship planet)
                             :speed (floor hlt:+max-speed+ 2)))
                        (loop-finish)))
        ;; Send all the issued commands to the game server.
        (hlt:finalize-turn *game*)))))
