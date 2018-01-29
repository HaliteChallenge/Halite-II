(ns hlt.log
  (:require
   [clojure.java.io :as io])
  (:import
   (java.text SimpleDateFormat)))

(def config (atom {}))

(defn- format-timestamp [d]
  (.format (SimpleDateFormat. "yyyyMMdd'T'HHmm") d))

(defn init
  [{:keys [base-dirname player-id started-at bot-name] :as init-config}]
  (let [dirname (format "%s/%s.%d" base-dirname
                           (format-timestamp started-at) player-id)
        logfile (format "%s/%s-log.edn" dirname bot-name)]
    (let [logdir (io/file dirname)]
      (when (not (.exists logdir)) (.mkdirs logdir)))
    (reset! config (merge {::logfile logfile ::dirname dirname}
                          init-config))))

(defn log-game-map
  [turn game-map]
  (let [{:keys [hlt.log/dirname bot-name]} @config
        game-map-file (format "%s/%s-game-map-%03d.edn" dirname bot-name turn)]
    (spit game-map-file game-map)))

(defn log
  [& keyvals]
  (when-let [logfile (::logfile @config)]
    (with-open [logger (io/writer logfile :append true)]
      (let [keyvals-map (apply array-map keyvals)]
        (.write logger (prn-str keyvals-map))))))
