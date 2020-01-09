(ns app.rename
  (:require [app.utils :refer :all]
            [me.raynes.fs :as fs]))

(def folder "resources/Season 8/")

(defn checking-film
  []
  (->> (str folder "Film")
       (fs/list-dir)
       (mapv str)
       (mapv fs/name)
       sort))

(defn checking-subtitle
  []
  (let [files (->> (str folder "Subtitle")
                   (fs/list-dir)
                   (mapv str)
                   sort
                   rest
                   vec)
        names (checking-film)
        parent (str (fs/parent (first files)))]
    (doseq [f (map #(do [%1 %2]) files names)]
      (let [[a b] f]
        (println (str parent "/" b ".srt"))
        (fs/copy a (str parent "/" b ".srt"))))))




