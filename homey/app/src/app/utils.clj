(ns app.utils
  (:require
    [clojure.edn :as edn]
    [clojure.pprint :refer [pprint]]))

(defn cslurp
  "Helper function to easily slurp and read-string a file"
  [fname]
  ((comp edn/read-string slurp) fname))

(defn cspit
  "Helper function to beautifully print clojure to file"
  [fname data]
  (->> data pprint with-out-str (spit fname)))






