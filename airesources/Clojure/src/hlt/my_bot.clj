(ns hlt.my-bot
  (:require
   [hlt.bot :as bot]
   [hlt.entity :as e]
   [hlt.navigation :as navigation]))

(defn compute-move
  [ship planets game-map]
  (if (not= (-> ship :docking :status) :undocked)
    nil
    (first
     (for [planet (vals planets)
           :when (nil? (:owner-id planet))]
       (if (e/within-docking-range? ship planet)
         (e/dock-move ship planet)
         (navigation/navigate-to-dock ship planet game-map))))))

(defrecord MyBot [bot-name player-id map-dims initial-map]
  bot/Bot
  (-name [this]
    (str bot-name "-" player-id))
  (-next-moves [this game-map]
    (let [ships (vals (get-in game-map [:owner-ships player-id]))
          planets (:planets game-map)]
      (keep #(compute-move % planets game-map) ships))))

(defn bot
  [bot-name {:keys [player-id map-size]} initial-map]
  (->MyBot bot-name player-id map-size initial-map))
