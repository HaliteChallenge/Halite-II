(ns hlt.bot
  (:refer-clojure :exclude [name]))

(defprotocol Bot
  (-name [bot])
  (-next-moves [bot game-map]))

(defn name
  "Returns the name of the bot"
  [bot]
  (-name bot))

(defn next-moves
  [bot game-map]
  (-next-moves bot game-map))
