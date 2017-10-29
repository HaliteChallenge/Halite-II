(ns hlt.utils
  (:require [clojure.string :as str]))

(def ^:dynamic *logger* nil)

(defn log
  "Logs the arguments to the log file."
  [& args]
  (.write *logger* (str/join " " args))
  (.write *logger* "\n")
  (.flush *logger*))

