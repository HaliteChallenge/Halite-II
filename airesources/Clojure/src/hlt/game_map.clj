(ns hlt.game-map)

(def ^:dynamic *bot-name*
  "The bot's name"
  nil)

(def ^:dynamic *player-id*
  "This bot's player id"
  nil)

(def ^:dynamic *map-size*
  "The size of the map as a vector [x y]"
  nil)

(def ^:dynamic *ships*
  "A map of ship-ids to the actual ship"
  nil)

(def ^:dynamic *owner-ships*
  "A map of player-ids to a ship-map, just as *ships*"
  nil)

(def ^:dynamic *planets*
  "A map from planet id to the actual planet"
  nil)
