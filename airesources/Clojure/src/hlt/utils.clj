(ns hlt.utils
  (:require [clojure.string :as str]))

(def ^:dynamic *logger* nil)

(defn log
  "Logs the arguments to the log file, as if printed by println.
  Multiple arguments are separated by spaces."
  [& args]
  (binding [*out* *logger*]
    (apply println args)))
