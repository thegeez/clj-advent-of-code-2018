(ns advent2018.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :as test]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]))

(defn day1-1 []
  (let [in (io/resource "day1.edn")
        nums (into []
                   (comp (map edn/read-string))
                   (xio/lines-in in))]
    (reduce + nums)))

(defn day1-2 []
  (let [in (io/resource "day1.edn")
        nums (into []
                   (comp (map edn/read-string))
                   (xio/lines-in in))]
    (reduce
     (fn [{:keys [seen freq] :as acc} num]
       (let [freq (+ freq num)]
         (if (contains? seen freq)
           (reduced freq)
           {:seen (conj seen freq)
            :freq freq})))
     {:seen #{}
      :freq 0}
     (cycle nums))))

(defn day1-2 []
  (let [in (io/resource "day1.edn")
        nums (into []
                   (comp (map edn/read-string))
                   (xio/lines-in in))]
    (reduce
     (fn [{:keys [seen freq] :as acc} num]
       (let [freq (+ freq num)]
         (if (contains? seen freq)
           (reduced freq)
           {:seen (conj seen freq)
            :freq freq})))
     {:seen #{}
      :freq 0}
     (cycle nums))))


(comment
  (day1-1) ;; 574
  (day1-2) ;; 452
  )


(defn day2-1 []
  (let [in (io/resource
            "day2.edn"
            ;;"day2_test.edn"
            )
        lines (xio/lines-in in)
        tally-freqs (fn [s]
                      (let [freqs (frequencies s)
                            count-freqs (frequencies (vals freqs))]
                        (select-keys count-freqs [2 3])))
        freqs (into []
                    (comp (map tally-freqs)
                          (map (fn [freqs]
                                 (zipmap (keys freqs)
                                         (repeat 1)))))
                    lines)
        total-freqs (apply merge-with
                           +
                           freqs)]
    (* (get total-freqs 2)
       (get total-freqs 3))))

(defn string-in [s-or-in]
  (if (string? s-or-in)
    (java.io.StringReader. s-or-in)
    s-or-in))

(defn day2-2 [in]
  (let [lines (into [] (xio/lines-in (string-in in)))
        same-ish (fn [l r]
                   (loop [[lc & lr] l
                          [rc & rr] r
                          one-diff-seen false]
                     (if (not (and lc rc))
                       ;; at the end
                       (if one-diff-seen
                         true
                         false)
                       (if (= lc rc)
                         (recur lr
                                rr
                                one-diff-seen)
                         (if one-diff-seen
                           false
                           (recur lr
                                  rr
                                  true ;; one-diff-seen
                                  ))))))
        res (-> (for [l lines
                      r lines
                      :when (same-ish l r)]
                  (->> (map list l r)
                       (keep (fn [[cl cr]]
                               (when (= cl cr)
                                 cl)))
                       (apply str)))
                first)]
    res))

(test/deftest day2-2-test
  (test/is (= (day2-2 "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
") "fgij")))

(comment
  (day2-1)
  (time (day2-2 (io/resource
                 "day2.edn"
                 ;;"day2_test2.edn"
                 )))

  (day2-2-test)
  )

(defn day3-1 [in]
  (let [claims (xio/lines-in (string-in in))
        claims (eduction
                (map (fn [line]
                       (let [[_pound id-str _space _at left-str top-str _colon width-x-height-str] (str/split line #"\W")
                             [width-str height-str] (str/split width-x-height-str #"x")
                             [id left top width height] (map #(Long/parseLong %) [id-str left-str top-str width-str height-str])
                             ]
                         [id left top width height]
                         )))
                claims)
        tiles (transduce
               (mapcat
                (fn [[id left top width height]]
                  (for [l (range left (+ left width))
                        t (range top (+ top height))]
                    [l t])))
               (fn
                 ([acc] acc)
                 ([acc tile]
                  (update acc tile (fnil inc 0))))
               {}
               claims)]
    (->> tiles
         vals
         (filter (fn [cnt]
                   (< 1 cnt)))
         count)
    ))

(test/deftest day3-1-test
  (test/is (= (day3-1 "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")
              4)))

(defn day3-2 [in]
  (let [claims (xio/lines-in (string-in in))
        claims (into []
                     (map (fn [line]
                            (let [[_pound id-str _space _at left-str top-str _colon width-x-height-str] (str/split line #"\W")
                                  [width-str height-str] (str/split width-x-height-str #"x")
                                  [id left top width height] (map #(Long/parseLong %) [id-str left-str top-str width-str height-str])
                                  ]
                              [id left top width height]
                              )))
                     claims)
        id->claim (into {}
                        (map (fn [[id left top width height]]
                               [id [left top width height]]))
                        claims)
        tiles (transduce
               (mapcat
                (fn [[id left top width height]]
                  (for [l (range left (+ left width))
                        t (range top (+ top height))]
                    [id [l t]])))
               (fn
                 ([acc] acc)
                 ([acc [id tile]]
                  (update acc tile (fnil conj #{}) id)))
               {}
               claims)
        tiles-per-id (into #{}
                           (comp (keep (fn [[tile ids]]
                                         (when (= (count ids) 1)
                                           [tile (first ids)])))
                                 (x/by-key second
                                           first
                                           (x/into [])))
                           tiles)
        result (some
                (fn [[id tiles]]
                  (let [claim (get id->claim id)
                        [left top width height] claim
                        needed-tiles (for [l (range left (+ left width))
                                           t (range top (+ top height))]
                                       [l t])]
                    (when (= (set tiles)
                             (set needed-tiles))
                      id)))
                tiles-per-id)]
    result))

(test/deftest day3-2-test
  (test/is (= (day3-2 "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2")
              3)))

(comment
  (day3-1 (io/resource
           "day3.txt")) ;; 110195
  (day3-1-test)

  (str/split "#1 @ 1,3: 4x4" #"\W")["" "1" "" "" "1" "3" "" "4x4"]
  (str/split "4x4" #"x")


  (day3-2-test)
  (day3-2 (io/resource
           "day3.txt")) ;; 894
  )


(defn sleep-minutes [in]
  (let [notes (xio/lines-in (string-in in))
        recs (into []
                   (map (fn [line]
                          (let [[_all year month day hour minute note :as all] (re-find #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)" line)
                                [year month day hour minute] (map #(Long/parseLong %) [year month day hour minute])
                                note (cond
                                       (= note "wakes up")
                                       {:action :wakes-up}
                                       (= note "falls asleep")
                                       {:action :falls-asleep}
                                       :else
                                       (let [guard-id (->> (re-find #"Guard #(\d+) begins shift" note)
                                                           second
                                                           Long/parseLong)]
                                         {:id guard-id}))]
                            [[year month day hour minute] note]
                            )))
                   notes)
        recs (sort-by first compare recs)
        add-guard-to-lines (keep (let [last-id (volatile! nil)]
                                   (fn [[date {:keys [action id]}]]
                                     (if id
                                       (do (vreset! last-id id)
                                           nil)
                                       [date {:action action
                                              :id @last-id}]))))

        recs (into {}
                   (comp add-guard-to-lines
                         (x/partition 2 2)
                         (map (fn [[[date-sleep {action-sleep :action
                                                 id-sleep :id} :as sleep]
                                    [date-awake {action-awake :action
                                                 id-awake :id} :as awake]]]
                                (assert (= id-sleep id-awake))
                                (let [[year-sleep month-sleep day-sleep hour-sleep min-sleep] date-sleep
                                      [year-awake month-awake day-awake hour-awake min-awake] date-awake
                                      _ (assert (= [year-sleep month-sleep day-sleep hour-sleep]
                                                   [year-awake month-awake day-awake hour-awake]))
                                      sleep-minutes (for [m (range min-sleep min-awake)]
                                                      [year-sleep month-sleep day-sleep hour-sleep m])]
                                  {:id id-sleep
                                   :sleep-minutes sleep-minutes})))
                         (x/by-key :id
                                   :sleep-minutes
                                   (comp cat
                                         (x/into []))))
                   recs)]
    recs))

(defn day4-1 [in]
  (let [recs (sleep-minutes in)
        [id sleep-minutes] (apply max-key (comp count val) recs)
        most-sleep (->> sleep-minutes
                        (map (fn [[year month day hour minute]]
                               minute))
                        frequencies
                        (apply max-key val)
                        first)
        result (* id most-sleep)]
    result
    ))

(test/deftest day4-1-test
  (let [in "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
        shuffled (->> in
                      (str/split-lines)
                      shuffle)
        s (str/join "\n" shuffled)]
    (test/is (= (day4-1 s)
                (* 10 24)))))


(defn day4-2 [in]
  (let [recs (sleep-minutes in)
        per-guard (for [[id sleep-minutes] recs]
                    (let [[min-most freq-most] (->> sleep-minutes
                                                    (map (fn [[year month day hour minute]]
                                                           minute))
                                                    frequencies
                                                    (apply max-key val))]
                      [id min-most freq-most]))

        [most-id most-min freq-most] (apply max-key last per-guard)
        result (* most-id most-min)]
    result
    ))

(test/deftest day4-2-test
  (let [in "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
        shuffled (->> in
                      (str/split-lines)
                      shuffle)
        s (str/join "\n" shuffled)]
    (test/is (= (day4-2 s)
                (* 99 45)))))


(comment
  (let [line "[1518-11-01 00:05] falls asleep"]
    (re-find
     #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.+)"
     line))
  (day4-1-test)

  (day4-1 (io/resource "day4.txt"))

  (day4-2-test)

  (day4-2 (io/resource "day4.txt"))

)

(defn match-opposite-case [l r]
  (or (and (Character/isLowerCase l)
           (Character/isUpperCase r)
           (= l (Character/toLowerCase r)))
      (and (Character/isLowerCase r)
           (Character/isUpperCase l)
           (= r (Character/toLowerCase l)))))

(defn day5-1 [in]
  (->> (reduce
        (fn [pre c]
          (let [end (peek pre)]
            (if (= c \newline)
              pre
              (if (and end
                       (match-opposite-case end c))
                (pop pre)
                (conj pre c)))))
        []
        in)
       (apply str)))

(test/deftest day5-1-test
  (test/is
   (= (day5-1 "dabAcCaCBAcCcaDA")
      "dabCBAcaDA"))
  (test/is
   (= (count (day5-1 "dabAcCaCBAcCcaDA")) 10))
  (test/are [in out] (= (day5-1 in) out)
    "aA" ""
    "abBA" ""
    "abAB" "abAB"
    "aabAAB" "aabAAB"
    "aYyzZxXZzAa" "a"))

(defn day5-2 [in]
  (let [candidates (mapv char (range (int \a) (inc (int \z))))
        rf (fn [pre c]
             (let [end (peek pre)]
               (if (= c \newline)
                 pre
                 (if (and end
                          (match-opposite-case end c))
                   (pop pre)
                   (conj pre c)))))
        streams (zipmap candidates
                        (map
                         (fn [filter-char]
                           (comp (remove #{filter-char (Character/toUpperCase filter-char)})
                                 (x/reduce (completing rf) [])
                                 (map count)))
                         candidates))]
    (->> (into {}
               (x/transjuxt streams)
               in)
         (apply min-key val)
         val)))

(test/deftest day5-2-test
  (test/is
   (= (day5-2 "dabAcCaCBAcCcaDA")
      4)))

(comment
  (day5-1-test)
  (-> (day5-1 (slurp (io/resource "day5.txt")))
      count) ;;11108

  (time (day5-2 (slurp (io/resource "day5.txt"))))5094
  
  (def r *1)
  (->> r
       (partition 2 1)
       (some (fn [[l r]]
               (when (match-opposite-case l r)
                 [l r]))))
  (count r)
  (day5-1 r)
  (def r2 *1)
  (count r2)
  
  (Character/isLowerCase \a)
  (Character/toLowerCase \A)
  (match-opposite-case \A \a)
  (match-opposite-case \a \A)
  (match-opposite-case \A \B)

  (into {} (x/transjuxt {:sum (x/reduce +) :mean x/avg :count x/count}) (range 256))
  )
