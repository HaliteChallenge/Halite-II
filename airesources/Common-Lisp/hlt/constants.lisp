(in-package :halite)

;; Max number of units of distance a ship can travel in a turn.
(defconstant +max-speed+ 7)

;; Radius of a ship.
(defconstant +ship-radius+ 0.5d0)

;; Maximum health of ship.
(defconstant +max-ship-health+ 255)

;; Starting health of ship.
(defconstant +base-ship-health+ 255)

;; Weapon cooldown period in game ticks.
(defconstant +weapon-cooldown+ 1)

;; Weapon damage radius.
(defconstant +weapon-radius+ 5.0d0)

;; Weapon damage.
(defconstant +weapon-damage+ 64)

;; Radius in which explosions affect other entities.
(defconstant +explosion-radius+ 10.0d0)

;; Distance from the edge of the planet at which ships can try to dock.
(defconstant +dock-radius+ 4.0d0)

;; Number of turns it takes to dock a ship.
(defconstant +dock-turns+ 4)

;; Number of production units per turn contributed by each docked ship.
(defconstant +base-productivity+ 6)

;; Distance from the planets edge at which new ships are created.
(defconstant +spawn-radius+ 2.0d0)
