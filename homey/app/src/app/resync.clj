(ns app.resync
  (:require [clj-time.core :as t]
            [clojure.string :as cs]
            [app.utils :refer :all]
            [me.raynes.fs :as fs]
            [clj-time.format :as f]))

(def base-folder "resources/LawOrder/")

(defn open-file
  [which-season which-one]
  (->> (str base-folder "Season" which-season "/" which-one ".srt")
       slurp))

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
  (let [totals (+ secs (* 60 mins))]
    (cond
      (< totals amount) 0
      (<= mins 13) (+ secs (* 60 mins))
      (< 13 mins 23) (- (+ (* 60 mins) secs) 3)
      (<= 23 mins 28) (- (+ (* 60 mins) secs) 4)
      (< 28 mins 33) (- (+ (* 60 mins) secs) 5)
      (<= 33 mins 38) (- (+ (* 60 mins) secs) 6)
      (< 38 mins 45) (- (+ (* 60 mins) secs) 7))))

(defn convert
  [time-string amount]
  (let [base (subs time-string 3 12)
        [mins sec12] (vec (cs/split base #":"))
        [secs sec2] (cs/split sec12 #",")
        int-mins (bigint mins)
        int-secs (bigint secs)
        allsecs (rules int-mins int-secs amount)]
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
  [which-season which-one amount]
  (let [all (->> (open-file which-season which-one)
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
  [which-season which-files amount]
  (doseq [dfile which-files]
    (println dfile)
    (spit (str base-folder "Season " which-season "/" dfile ".srt")
          (convert-all which-season dfile amount))))

