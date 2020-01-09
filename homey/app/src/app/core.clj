(ns app.core
  (:require
    [dk.ative.docjure.spreadsheet :as xl]
    [app.utils :refer :all]))

(defn loader
  "kelas is an integer from 1-12"
  [kelas]
  (->> (xl/load-workbook "resources/nodes/nodes.xlsx")
       (xl/select-sheet (str "Kelas " kelas))
       (xl/select-columns {:G "level1"
                           :H "level2"
                           :I "level3"
                           :J "level4"
                           :K "level5"
                           :L "level6"})))

(defn fmt [integer]
  (if (< integer 10)
    (str "0" integer)
    (str integer)))

(defn actor
  "This one takes the load and create a tree structure"
  [kelas]
  (let [loaded (->> (loader kelas)
                    (remove nil?)
                    (map-indexed #(assoc %2 :seq %1)))]
    (loop [level 1 result []]
      (if (> level 6)
        (vec (sort-by :seq result))
        (let [nama-level (str "level" level)
              res (->> loaded
                       (filter #(string? (get % nama-level)))
                       (mapv #(assoc (select-keys % [:seq])
                                :level level
                                :title (get % nama-level))))]
          (recur (inc level) (concat result res)))))))

(defn builder
  [kelas]
  (let [loaded (actor kelas)]
    (loop [[x & xs] loaded level 1 order 0 naming [] res []]
      (if x
        (let [loaded-level (:level x)]
          (cond
            (== loaded-level level)
            (let [code (str "K13R-"
                            (fmt kelas)
                            (apply str (map fmt (conj naming (inc order)))))]
              (recur xs
                     level
                     (inc order)
                     naming
                     (conj res (assoc x
                                 :code code
                                 :parent-code (apply str (drop-last 2 code))))))
            (> loaded-level level)
            (let [code (str "K13R-"
                            (fmt kelas)
                            (apply str (map fmt (conj naming order 1))))]
              (recur xs
                     loaded-level
                     1
                     (conj naming order)
                     (conj res (assoc x
                                 :code code
                                 :parent-code (apply str (drop-last 2 code))))))
            :else
            (let [new-naming (subvec naming 0 (dec loaded-level))
                  new-order (inc (last (take loaded-level naming)))
                  code (->> (conj new-naming new-order)
                            (map fmt)
                            (apply str)
                            (str "K13R-" (fmt kelas)))]
              (recur xs
                     loaded-level
                     new-order
                     new-naming
                     (conj res (assoc x
                                 :code code
                                 :parent-code (apply str (drop-last 2 code))))))))
        res))))

(defn act-all
  [folder]
  (doseq [x (range 1 13)]
    (println "Kelas " x)
    (cspit (str "resources/" folder "/" (fmt x) ".edn")
           (builder x))))

