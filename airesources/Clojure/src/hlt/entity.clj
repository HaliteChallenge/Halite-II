(ns hlt.entity
  (:require [hlt.math :as math :refer [get-x get-y]])
  (:import (hlt.math Positionable)))

(def ^:const max-players 4)
(def ^:const max-ship-speed 7)
(def ^:const ship-radius 0.5)
(def ^:const max-ship-health 255)
(def ^:const base-ship-health 255)
(def ^:const weapon-radius 5.0)
(def ^:const weapon-damage 64)
(def ^:const explosion-radius 10)
(def ^:const dock-radius 4.0)
(def ^:const dock-turns 5)
(def ^:const base-productivity 5)
(def ^:const spawn-radius 2.0)

(defrecord Ship [id pos health radius owner-id docking]
  Positionable
  (get-x [_] (get-x pos))
  (get-y [_] (get-y pos)))

(def dock-statuses
  "The different docking statuses a ship can have."
  #{:undocked :docking :docked :undocking})

(defrecord Planet [id pos health radius owner-id docking]
  Positionable
  (get-x [_] (get-x pos))
  (get-y [_] (get-y pos)))

(defn within-docking-range?
  "Returns whether a ship is within distance to dock on the provided
  planet."
  [ship planet]
  (<= (math/distance-between ship planet)
      (+ dock-radius (:radius planet))))

(defn remaining-docking-spots
  "Returns the number of remaining docking spots for this planet"
  [planet]
  (- (-> planet :docking :spots)
     (-> planet :docking :ships count)))

(defn any-remaining-docking-spots?
  "Returns whether there are any remaining docking spots on this planet"
  [planet]
  (pos? (remaining-docking-spots planet)))

(defn dock-move
  "Returns a docking move for this ship and planet."
  [ship planet]
  {:type :dock :ship ship :planet planet})

(defn undock-move
  "Returns an undocking move for this ship."
  [ship]
  {:type :undock :ship ship})

(defn thrust-move
  "Returns a thrust move for this ship."
  [ship thrust rad-angle]
  (let [deg-angle (math/rad->deg rad-angle)]
    {:type :thrust
     :ship ship
     :thrust thrust
     :angle deg-angle}))
