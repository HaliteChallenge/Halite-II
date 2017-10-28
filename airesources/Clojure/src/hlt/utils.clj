(ns hlt.utils
  (:require [clojure.string :as str]))

(def ^:dynamic *logger* nil)

(defn log
  "Logs the arguments to the log file."
  [& args]
  (.write *logger* (str/join " " args))
  (.write *logger* "\n")
  (.flush *logger*))

(defn rad->deg
  "Translates radians into degrees in the interval [0, 360)."
  [rad]
  (let [deg-unclipped (Math/round (Math/toDegrees rad))]
    ;; Clip to ensure value is in [0, 360), as required by the game engine.
    (int (mod deg-unclipped 360))))
