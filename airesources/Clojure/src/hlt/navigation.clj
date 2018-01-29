(ns hlt.navigation
  (:require
   [hlt.entity :as e]
   [hlt.math :as math :refer [get-x get-y]]))

;; The navigation code in this namespace is here to get you up and running. It
;; is nonessential and can be replaced with improved navigation if its
;; performance is bad, or you want to improve the navigation further.

(def max-navigation-corrections 90)
(def docking-distance 3.0)

(def default-fudge-factor (+ e/ship-radius 0.2))

(defn entities-between
  "Returns all the entities that intersects the line segment from a to b."
  [a b game-map]
  (let [{:keys [planets ships]} game-map
        filter-fn #(and (distinct? a b %)
                        (math/segment-circle-intersects? a b % default-fudge-factor))]
    (concat (filter filter-fn (vals planets))
            (filter filter-fn (vals ships)))))

(def ^:private default-navigation-opts
  {:max-corrections max-navigation-corrections
   :avoid-obstacles true
   :angular-step (/ Math/PI 180.0)
   :max-thrust (quot e/max-ship-speed 2)})

(defn navigate-to
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal game-map]
   (navigate-to ship goal game-map default-navigation-opts))
  ([ship goal game-map {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (if (<= max-corrections 0)
     nil
     (let [distance (math/distance-between ship goal)
           angle (math/orient-towards ship goal)]
       (if (and avoid-obstacles (seq (entities-between ship goal game-map)))
         (let [new-target-dx (* (Math/cos (+ angle angular-step)) distance)
               new-target-dy (* (Math/sin (+ angle angular-step)) distance)
               new-goal (math/->Position (+ new-target-dx (get-x ship))
                                         (+ new-target-dy (get-y ship)))]
           (recur ship new-goal game-map (update opts :max-corrections dec)))

         (let [thrust (int (min distance max-thrust))]
           (e/thrust-move ship thrust angle)))))))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  ([ship planet game-map]
   (navigate-to-dock ship planet game-map {}))
  ([ship planet game-map opts]
   (let [docking-point (math/closest-point ship planet docking-distance)]
     (navigate-to ship docking-point game-map
                  (merge default-navigation-opts opts)))))


(defn entities-by-distance
  "Returns a list of all entities sorted by distance to x. If x itself
  is an entity, it is not included in the list."
  [x game-map]
  (let [{:keys [planets ships]} game-map]
    (->> (concat planets ships)
         (map vals)
         (remove #{x})
         (sort-by #(math/distance-between x %)))))
