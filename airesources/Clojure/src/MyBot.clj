(ns MyBot
  (:require [clojure.java.io :as clj.io]
            [hlt.networking :as io]
            [hlt.game-map :refer [*player-id* *map-size* *bot-name*
                                  *owner-ships* *ships* *planets*]]
            [hlt.utils :as utils]
            [hlt.entity :as e]
            [hlt.navigation :as navigation])
  (:import (java.io PrintWriter))
  (:gen-class))

(defmacro with-updated-map
  [& body]
  `(let [m# (io/read-map)]
     (binding [*owner-ships* (:owner-ships m#)
               *ships* (:ships m#)
               *planets* (:planets m#)]
       ~@body)))

;; Here we set the bot name to Doohickey.
(def my-bot-name "Doohickey")
(defmacro initialize-game
  [& body]
  `(let [prelude# (io/read-prelude)
         bot-name# (str my-bot-name "-" (:player-id prelude#))]
     (with-open [logger# (clj.io/writer (str bot-name# ".log"))]
       (binding [utils/*logger* logger#
                 *bot-name* bot-name#
                 *player-id* (:player-id prelude#)
                 *map-size* (:map-size prelude#)]
         (try
           (with-updated-map ~@body)
           (catch Throwable t#
             (with-open [pw# (PrintWriter. utils/*logger*)]
               (.printStackTrace t# pw#))
             (throw t#)))))))

(defn compute-move
  [ship]
  (if (not= (-> ship :docking :status) :undocked)
    nil
    (first
     (for [planet (vals *planets*)
           :when (nil? (:owner-id planet))]
       (if (e/within-docking-range? ship planet)
         (e/dock-move ship planet)
         (navigation/navigate-to-dock ship planet))))))

(defn -main
  [& args]
  (initialize-game
   (io/send-done-initialized)
   (doseq [turn (iterate inc 1)]
     (with-updated-map
      (utils/log "=========== Turn" turn "===========")
      (let [moves (keep compute-move (vals (get *owner-ships* *player-id*)))]
        (io/send-moves moves))))))
