(ns hlt.networking
  (:require
   [clojure.string :as str]
   [hlt.entity :as e]
   [hlt.math :as math]))

(defn read-prelude
  [{:keys [in]}]
  (let [player-id (read in)
        width (read in)
        height (read in)]
    {:player-id player-id
     :map-size [width height]}))

;; decoding game map
(defn- to-docking-status
  [docking-status]
  (case docking-status
    0 :undocked
    1 :docking
    2 :docked
    3 :undocking))

(defn- parse-ship
  [stream owner-id]
  (let [ship-id          (read stream)
        x-loc            (read stream)
        y-loc            (read stream)
        health           (read stream)
        x-velocity       (read stream) ;; velocity, which is deprecated
        y-velocity       (read stream)
        docking-status   (to-docking-status (read stream))
        docked-planet    (read stream)
        docking-progress (read stream)
        weapon-cooldown  (read stream)] ;; weapon cooldown seems unused as well
    (e/->Ship ship-id (math/->Position x-loc y-loc) health e/ship-radius owner-id
              {:status docking-status
               :planet docked-planet
               :progress docking-progress})))

(defn- parse-ship-list
  [stream owner-id]
  (let [num-ships (read stream)]
    (loop [ships (transient {})
           cur-ship 0]
      (if (== num-ships cur-ship)
        (persistent! ships)
        (let [ship (parse-ship stream owner-id)]
          (recur (assoc! ships (:id ship) ship)
                 (inc cur-ship)))))))

(defn- parse-all-ships
  [stream]
  (let [num-players (read stream)]
     (loop [all-ships (transient {})
            owner-ships {}
            cur-player 0]
       (if (== num-players cur-player)
         {:owner-ships owner-ships
          :ships (persistent! all-ships)}
         (let [owner-id (read stream)
               ships (parse-ship-list stream owner-id)]
           (recur (reduce conj! all-ships ships)
                  (assoc owner-ships owner-id ships)
                  (inc cur-player)))))))

(defn- parse-planet
  [stream]
  (let [id                   (read stream)
        x-loc                (read stream)
        y-loc                (read stream)
        health               (read stream)
        radius               (read stream)
        docking-spots        (read stream)
        current-production   (read stream) ;; Deprecated
        remaining-production (read stream) ;;
        has-owner            (read stream)
        owner-candidate      (read stream)
        docked-ship-count    (read stream)
        docked-ships (vec (repeatedly docked-ship-count #(read stream)))]
    (e/->Planet id (math/->Position x-loc y-loc) health radius
                (if (== has-owner 1)
                  owner-candidate
                  nil)
                {:spots docking-spots
                 :ships docked-ships})))

(defn- parse-all-planets
  [stream]
  (let [num-planets (read stream)]
    (loop [planets (transient {})
           cur-planet 0]
      (if (== num-planets cur-planet)
        (persistent! planets)
        (let [planet (parse-planet stream)]
          (recur (assoc! planets (:id planet) planet)
                 (inc cur-planet)))))))

(defn read-map
  [{:keys [in]}]
  (let [ships   (parse-all-ships in)
        planets (parse-all-planets in)]
    (assoc ships :planets planets)))

;; finish initialization

(defn send-done-initialized
  "Notifies the game engine that this bot has been initialized."
  [{:keys [out]} bot-name]
  (doto out
    (.write (str bot-name "\n"))
    (.flush)))

;; encoding moves

(def ^:private undock-key "u")
(def ^:private dock-key "d")
(def ^:private thrust-key "t")

(defmulti move-segments :type)

(defmethod move-segments :undock [move]
  [undock-key (-> move :ship :id)])

(defmethod move-segments :dock [move]
  [dock-key (-> move :ship :id) (-> move :planet :id)])

(defmethod move-segments :thrust [move]
  [thrust-key (-> move :ship :id) (:thrust move) (:angle move)])

(defn send-moves
  "Sends the moves to the game engine."
  [{:keys [out]} moves & r]
  (let [move-seq (mapcat move-segments moves)]
    (doto out
      (.write (str (str/join " " move-seq) "\n"))
      (.flush))))
