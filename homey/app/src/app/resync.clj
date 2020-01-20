(ns app.resync
  (:require [clj-time.core :as t]
            [clojure.string :as cs]
            [app.utils :refer :all]
            [me.raynes.fs :as fs]
            [clj-time.format :as f]))

(def base-folder "resources/LawOrder/")

(defn open-file
  [which-season which-one]
  (let [res (->> (str base-folder "Season" which-season "/" which-one ".srt")
                 slurp)]
    res))

(defn separate
  [txt]
  (cs/split txt #"-->"))

(defn dform
  [some-number]
  (if (< some-number 10)
    (str "0" some-number)
    (str some-number)))

(defn rules
  [mins secs amount]
  (let [totals (+ secs (* 60 mins))
        res (cond
              (< totals amount) 0
              (<= mins 15) (- (+ secs (* 60 mins)) 2)
              (< 15 mins 20) (- (+ (* 60 mins) secs) 3)
              (<= 20 mins 25) (- (+ (* 60 mins) secs) 4)
              (<= 25 mins 30) (- (+ (* 60 mins) secs) 5)
              (<= 30 mins 35) (- (+ (* 60 mins) secs) 5)
              (< 35 mins 45) (- (+ (* 60 mins) secs) 5))]
    res))

(defn convert
  [time-string amount]
  (let [base (subs time-string 3 12)
        [mins sec12] (vec (cs/split base #":"))
        [secs sec2] (cs/split sec12 #",")
        int-mins (bigint mins)
        int-secs (bigint secs)
        allsecs (rules int-mins int-secs amount)
        res (str "00:"
                 (dform (quot allsecs 60))
                 ":"
                 (dform (rem allsecs 60))
                 ","
                 sec2)]
    res))

(defn convert-both
  [time-string amount]
  (let [[a b] (->> (separate time-string)
                   (mapv cs/trim))
        res (str (convert a amount) " --> " (convert b amount))]
    res))

(defn work
  [line amount]
  (let [res (update-in line
                       [1]
                       #(convert-both % amount))]
    res))

(defn convert-all
  [which-season which-one amount]
  (let [all (->> (open-file which-season which-one)
                 (cs/split-lines)
                 (partition-by #(= "" %))
                 (remove #(= [""] %))
                 (mapv vec)
                 (mapv #(work % amount)))
        res (->> all
                 (mapv #(interpose "\r\n" %))
                 (mapv #(conj (vec %) "\r\n" "\r\n"))
                 (apply concat)
                 (apply str))]
    res))

(defn do-all-files
  [which-season which-files amount]
  (doseq [dfile which-files]
    (println dfile)
    (spit (str base-folder "Season " which-season "/" dfile ".srt")
          (try (convert-all which-season dfile amount)
               (catch Exception e
                 (println "Error " (.getMessage e)))))))

