(ns hlt.engine
  (:require [hlt.game-map :refer [*ships* *owner-ships* *planets* *player-id*]]
            [hlt.utils :as utils]))

(def ^:const max-players 4)
(def ^:const max-speed 7)
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
(def max-navigation-corrections 90)
(def docking-distance 3.0)
(def default-fudge-factor (+ ship-radius 0.2))

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
  (let [deg-angle (utils/rad->deg rad-angle)]
    {:type :thrust
     :ship ship
     :thrust thrust
     :angle deg-angle}))

(def dock-statuses
  "The different docking statuses a ship can have."
  #{:undocked :docking :docked :undocking})

(defprotocol Positionable
  "Positionable is either a location in the game map, or an entity with a
  location in the game map."
  (get-x [this] "Returns the center x-coordinate for this object")
  (get-y [this] "Returns the center y-coordinate for this object"))

(defrecord Position [x y]
  Positionable
  (get-x [_] x)
  (get-y [_] y)
  Object
  (toString [_] (format "<%s, %s>" x y)))

(defn square [x]
  (* x x))

(defn distance-between
  "Returns the distance between the center of a and b."
  [a b]
  (let [dx (- (get-x a) (get-x b))
        dy (- (get-y a) (get-y b))]
    (Math/sqrt (+ (square dx)
                  (square dy)))))

(defn orient-towards
  "Returns the angle from `from` to `to` in radians."
  [from to]
  (let [dx (- (get-x to) (get-x from))
        dy (- (get-y to) (get-y from))]
    (+ (Math/atan2 dy dx)
       (* 2 Math/PI))))

(defn closest-point
  "Returns the closest safe point from `point` towards `goal`. The
  closest safe point is on the radius of `goal` plus an optioanl
  distance, which can be used to avoid collisions."
  ([point goal] (closest-point point goal 0.0))
  ([point goal safe-distance]
   (let [radius (+ (:radius goal) safe-distance)
         angle (orient-towards goal point)
         x (+ (get-x goal) (* radius (Math/cos angle)))
         y (+ (get-y goal) (* radius (Math/sin angle)))]
     (->Position x y))))

(defn segment-circle-intersects?
  "Returns true if the entity intersects with the line segment between p1 and p2,
  false otherwise."
  [p1 p2 entity]
  ;; Parameterize the segment as start + t * (end - start),
  ;; and substitute into the equation of a circle
  ;; Solve for t
  (let [fudged-radius (+ (:radius entity) default-fudge-factor)
        x1 (get-x p1)
        y1 (get-y p1)
        x2 (get-x p2)
        y2 (get-y p2)
        dx (- x2 x1)
        dy (- y2 y1)
        a (+ (square dx) (square dy))

        center-x (get-x entity)
        center-y (get-y entity)

        b (* -2 (+ (square x1) (- (* x1 x2))
                   (- (* x1 center-x)) (* center-x x2)
                   (square y1) (- (* y1 y2))
                   (- (* y1 center-y)) (* center-y y2)))]
    (if (== a 0.0)
      ;; start == end
      (<= (distance-between p1 entity) fudged-radius)
      ;; time along segment when closest to the circle (vertex of the quadratic)
      (let [t (min (/ (- b) (* 2 a)) 1.0)]
        (if (< t 0)
          false
          (let [closest-x (+ x1 (* dx t))
                closest-y (+ y1 (* dy t))
                closest-distance (distance-between (->Position closest-x closest-y)
                                                   entity)]
            (<= closest-distance fudged-radius)))))))

(defn entities-between
  "Returns all the entities that intersects the line segment from a to b."
  [a b]
  (let [filter-fn #(and (distinct? a b %)
                        (segment-circle-intersects? a b %))]
    (concat (filter filter-fn (vals *planets*))
            (filter filter-fn (vals (get *owner-ships* *player-id*))))))

(def ^:private default-navigation-opts
  {:max-corrections max-navigation-corrections
   :avoid-obstacles true
   :angular-step (/ Math/PI 180.0)
   :max-thrust (quot max-speed 2)})

(defn navigate-to
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (if (<= max-corrections 0)
     nil
     (let [distance (distance-between ship goal)
           angle (orient-towards ship goal)]
       (if (and avoid-obstacles (seq (entities-between ship goal)))
         (let [new-target-dx (* (Math/cos (+ angle angular-step)) distance)
               new-target-dy (* (Math/sin (+ angle angular-step)) distance)
               new-goal (->Position (+ new-target-dx (get-x ship))
                                    (+ new-target-dy (get-y ship)))]
           (recur ship new-goal (update opts :max-corrections dec)))

         (let [thrust (int (min distance max-thrust))]
           (thrust-move ship thrust angle)))))))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  ([ship planet]
   (navigate-to-dock ship planet {}))
  ([ship planet opts]
   (let [docking-point (closest-point ship planet docking-distance)]
     (navigate-to ship docking-point
                  (merge default-navigation-opts opts)))))

(defrecord Ship [id pos health radius owner-id docking]
  Positionable
  (get-x [_] (get-x pos))
  (get-y [_] (get-y pos))
  Object
  (toString [this] (pr-str this)))

(defrecord Planet [id pos health radius owner-id docking]
  Positionable
  (get-x [_] (get-x pos))
  (get-y [_] (get-y pos))
  Object
  (toString [this] (pr-str this)))

(defn can-dock?
  "Returns whether a ship is within distance to dock on the provided
  planet."
  [ship planet]
  (<= (distance-between (:pos ship) (:pos planet))
      (+ dock-radius (:radius planet))))
