(ns halite-clj.bot
  (:require    [halite-clj.game :as game]
               [halite-clj.io :as io])
  (:gen-class))

(def bot-name "MyFirstClojureBot")

(defn random-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (let [my-sites (->> game-map
                      flatten
                      (filter #(= (:owner %) my-id)))]
    (map vector my-sites (repeatedly #(rand-nth game/directions)))))

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (io/send-moves! (random-moves my-id game-map))))))
