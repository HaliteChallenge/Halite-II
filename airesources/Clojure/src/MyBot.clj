(ns MyBot
  (:require
   [clojure.java.io :as clj.io]
   [hlt.bot :as bot]
   [hlt.log :as log]
   [hlt.my-bot :as bot-impl]
   [hlt.networking :as io])
  (:import
   (java.io PrintWriter))
  (:gen-class))

;; Here we set the bot name to Doohickey.
(def my-bot-name "Doohickey")

(defn -main
  [& args]
  (let [io          {:in *in* :out *out*}
        prelude     (io/read-prelude io)
        initial-map (io/read-map io)
        bot         (bot-impl/bot my-bot-name prelude initial-map)
        bot-name    (bot/name bot)]
    (log/init {:bot-name     bot-name
               :player-id    (:player-id prelude)
               :started-at   (java.util.Date.)
               :base-dirname "log"})
    (io/send-done-initialized io bot-name)
    (loop [turn 0]
      (log/log :turn turn)
      (try
        (let [game-map (io/read-map io)
              moves    (bot/next-moves bot game-map)]
          (io/send-moves io moves))
        (catch Throwable t
          (with-open [pw (PrintWriter. *out*)]
            (.printStackTrace t pw))
          (throw t)))
      (recur (inc turn)))))
