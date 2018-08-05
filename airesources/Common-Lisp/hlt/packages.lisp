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
   #:game
   #:make-game
   #:send-bot-name
   #:finalize-turn
   #:make-position
   #:make-relative-position
   #:nth-player
   #:angle-between
   #:distance
   #:closest-position
   #:path-intersects-entity-p
   #:nearby-entities
   #:nearby-entities-by-distance
   #:obstacles-between
   #:ships-between
   #:planets-between
   #:ship-docking-p
   #:planet-owned-p
   #:id
   #:game-map
   #:pos-x
   #:pos-y
   #:command
   #:ships
   #:radius
   #:health
   #:owner
   #:production
   #:resources
   #:docking-spots
   #:docked-ships
   #:vel-x
   #:vel-y
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
