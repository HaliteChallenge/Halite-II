(ns halite-clj.game)

;; The map is represented by a 2D vector of Sites
(defrecord Site [^int x ^int y ^int production ^int strength ^int owner])

(def directions [:still :north :east :south :west])
(def cardinal-directions (rest directions))

(defn adjacent-site [game-map site direction]
  (let [width (count (first game-map))
        height (count game-map)
        new-x (condp = direction
                :west (mod (dec (:x site)) width)
                :east (mod (inc (:x site)) width)
                (:x site))
        new-y (condp = direction
                :north (mod (dec (:y site)) height)
                :south (mod (inc (:y site)) height)
                (:y site))]
    (get-in game-map [new-y new-x])))

(defn single-dimension-distance
  "Computes the distance between two integers mod m"
  [p1 p2 m]
  (let [unwrapped-distance (Math/abs (- p1 p2))]
    (min unwrapped-distance (- m unwrapped-distance))))

(defn distance [game-map site1 site2]
  (+ (single-dimension-distance (:x site1) (:x site2) (count (first game-map)))
     (single-dimension-distance (:y site1) (:y site2) (count game-map))))
