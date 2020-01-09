(ns app.resync
  (:require [clj-time.core :as t]
            [clojure.string :as cs]
            [app.utils :refer :all]
            [me.raynes.fs :as fs]
            [clj-time.format :as f]))

(defn open-file
  [which-one]
  (->> (str "resources/Season3/" which-one ".srt")
       slurp))

(defn separate
  [txt]
  (cs/split txt #"-->"))

(defn dform
  [some-number]
  (if (< some-number 10)
    (str "0" some-number)
    (str some-number)))

(defn convert
  [time-string amount]
  (let [base (subs time-string 3 12)
        [mins sec12] (vec (cs/split base #":"))
        [secs sec2] (cs/split sec12 #",")
        allseconds (+ (* 60 (bigint mins)) (bigint secs))
        allsecs (if (> allseconds amount)
                  (- allseconds amount)
                  0)]
    (str "00:"
         (dform (quot allsecs 60))
         ":"
         (dform (rem allsecs 60))
         ","
         sec2)))

(defn convert-both
  [time-string amount]
  (let [[a b] (->> (separate time-string)
                   (mapv cs/trim))]
    (str (convert a amount) " --> " (convert b amount))))

(defn work
  [line amount]
  (update-in line
             [1]
             #(convert-both % amount)))

(defn convert-all
  [which-one amount]
  (let [all (->> (open-file which-one)
                 (cs/split-lines)
                 (partition-by #(= "" %))
                 (remove #(= [""] %))
                 (mapv vec)
                 (mapv #(work % amount)))]
    (->> all
         (mapv #(interpose "\r\n" %))
         (mapv #(conj (vec %) "\r\n" "\r\n"))
         (apply concat)
         (apply str))))

(defn do-all-files
  [which-files amount]
  (doseq [dfile which-files]
    (println dfile)
    (spit (str "resources/Season 3/" dfile ".srt")
          (convert-all dfile amount))))