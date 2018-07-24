(in-package :cl-user)

(defpackage :halite
  (:nicknames :hlt)
  (:use :common-lisp)
  (:export
   ;; Constants
   #:+max-speed+
   #:+ship-radius+
   #:+max-ship-health+
   #:+base-ship-health+
   #:+weapon-cooldown+
   #:+weapon-radius+
   #:+weapon-damage+
   #:+explosion-radius+
   #:+dock-radius+
   #:+dock-turns+
   #:+base-productivity+
   #:+spawn-radius+

   ;; Functions
   #:make-game
   #:finalize-turn
   #:game
   #:angle-between
   #:distance
   #:closest-point-to
   #:nearby-entities
   #:nearby-entities-by-distance
   #:ship-docking-p
   #:planet-owned-p
   #:nth-player
   #:id
   #:ships
   #:x
   #:y
   #:radius
   #:health
   #:owner
   #:production
   #:resources
   #:docking-spots
   #:docked-ships
   #:x-velocity
   #:y-velocity
   #:docking-status
   #:planet
   #:progress
   #:weapon-cooldown
   #:players
   #:planets
   #:width
   #:height
   #:user-id
   #:bot-name
   #:initial-map
   #:current-map

   ;; Commands
   #:issue-dock-command
   #:issue-undock-command
   #:issue-move-command
   #:issue-navigate-command
   ))
