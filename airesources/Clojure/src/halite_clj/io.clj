(ns halite-clj.io
  (:require clojure.string
            halite-clj.game))

(defn create-game-map
  "Parses the run-length encoded format of the map into a 2D vector of Sites"
  [width height productions compressed-map]
  (let [total-size (* width height)
        potential-owner-run-pairs (partition 2 compressed-map)
        owner-pair-count (->> potential-owner-run-pairs
                              (map first)
                              (reductions + 0)
                              (take-while #(not= total-size %))
                              (count))
        owners (->> potential-owner-run-pairs
                    (take owner-pair-count)
                    (mapcat #(repeat (first %) (second %))))
        strengths (->> compressed-map
                       (drop (* 2 owner-pair-count)))
        flat-sites (map halite-clj.game/->Site
                        (cycle (range width))
                        (mapcat #(repeat width %) (range))
                        productions
                        strengths
                        owners)]
    (mapv vec (partition width flat-sites))))

(defn read-ints!
  "Reads a sequence of space-delimited integers from *in*"
  []
  (map #(Integer/parseInt %)
       (clojure.string/split (read-line) #" ")))

(defn get-init!
  "Reads all the initialization data provided by the Halite environment process"
  []
  (let [my-id (Integer/parseInt (read-line))
        [width height] (read-ints!)
        productions (read-ints!)
        game-map (create-game-map width height productions (read-ints!))]
    {:my-id my-id :width width :height height :productions productions :game-map game-map}))


(def direction->int (zipmap halite-clj.game/directions (range)))

(defn- format-moves-for-output [moves]
  (clojure.string/join " "
                       (for [[site direction] moves]
                         (clojure.string/join " " [(:x site) (:y site) (direction->int direction)]))))

(defn send-moves!
  "Submits a list of [site, direction] pairs to the Halite enviroment process"
  [moves]
  (println (format-moves-for-output moves)))
