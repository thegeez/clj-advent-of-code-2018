(ns advent2018.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]
            [net.cgrand.xforms.rfs :as xrf]))

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

(set! *warn-on-reflection* true)

(defn match-opposite-case [^Character l ^Character r]
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
                           (comp (remove #{filter-char (Character/toUpperCase ^Character filter-char)})
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

(defn day5-2-alt [in]
  (let [candidates (mapv char (range (int \a) (inc (int \z))))
        rf (fn [pre c]
             (let [end (peek pre)]
               (if (= c \newline)
                 pre
                 (if (and end
                          (match-opposite-case end c))
                   (pop pre)
                   (conj pre c)))))
        streams (transduce
                 (map
                  (fn [filter-char]
                    (x/some
                     (comp (remove #{filter-char (Character/toUpperCase ^Character filter-char)})
                           (x/reduce (completing rf) [])
                           (map count))
                     in)))
                 xrf/min
                 candidates)]
    streams))

(comment
  (day5-1-test)
  (-> (day5-1 (slurp (io/resource "day5.txt")))
      count)

  (time (day5-2 (slurp (io/resource "day5.txt"))))5094

  (let [in (slurp (io/resource "day5.txt"))]
    (time (day5-2 in)))
  (let [in (slurp (io/resource "day5.txt"))]
    (time
     (day5-2-alt in)))

  (require '[clj-async-profiler.core :as prof])

  (let [in (slurp (io/resource "day5.txt"))]
    (prof/profile (day5-2 in)))
  (prof/profile (dotimes [i 10000] (reduce + (range i))))
  
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


(defn day6-1 [in]
  (let [lines (into []
                    (map-indexed
                     (fn [idx line]
                       (let [[_ x y] (re-find #"(\d+), (\d+)" line)
                             [x y] (map #(Long/parseLong %) [x y])
                             id (if (<= idx 24)
                                  (char (+ (int \a) idx))
                                  (str "ID_" idx))]
                         [[x y] id])))
                    (xio/lines-in (string-in in)))

        ;; points spawn 'claimers' which spawn claimers in 8 directions
        ;; claimer dies, claims on same char die, claims on other claim die and become dot .

        maxs (into {}
                   (x/transjuxt {:x-max (comp (map (fn [[[x y] id]]
                                                     x))
                                              x/max)
                                 :y-max  (comp (map (fn [[[x y] id]]
                                                      y))
                                               x/max)})
                   lines)
        max-gen (max (:x-max maxs) (:y-max maxs))

        spawn-to (fn [state]
                   (->> (for [[[x y] id] (:active state)
                              [dx dy] [[0 -1] [0 1] [-1 0] [1 0]]
                              :let [nx (+ x dx)
                                    ny (+ y dy)]]
                          [[nx ny] id])
                        (reduce
                         (fn [spawned [[x y] id]]
                           (let [claim-id (get spawned [x y])]
                             (if (and claim-id
                                      (not= id claim-id))
                               ;; concurrent claim is shared dot \.
                               (assoc spawned [x y] \.)
                               (assoc spawned [x y] id))))
                         {})))
        
        state (reduce
               (fn [state gen]
                 (if (= gen max-gen)
                   (reduced state)
                   (let [spawned (spawn-to state)]
                     (reduce
                      (fn [state [[x y] id]]
                        (if-let [claim-id (get (:field state) [x y])]
                          ;; if our own or someone else -> die
                          state
                          (-> state
                              (assoc-in [:field [x y]] id)
                              (update :active assoc [x y] id))))
                      (-> state
                          (update :gen inc)
                          (assoc :active {}))
                      spawned))))
               {:gen 0
                :active (into {} lines)
                :field (into {}
                             (map (fn [[xy id]]
                                    [xy (if (char? id)
                                          (Character/toUpperCase ^Character id)
                                          id)]))
                             lines)}
               (range))
        result (let [sources (map (fn [[xy id]] id) lines)
                     active-sources (set (map (fn [[xy id]] id) (:active state)))
                     sources (set (remove active-sources sources))]
                 (println "sources" sources "active-sources" active-sources)
                 (->> (into {:none 0}
                            (comp (keep (fn [[xy id]]
                                          (when (contains? sources id)
                                            id)))
                                  (x/by-key identity
                                            x/count))
                            (:field state))
                      (apply max-key second)))
        state (assoc state :result result)]
    state))

(defn print-state6 [state]
  (let [field (:field state)]
    (println "at generation: " (:gen state))
    (println "active" (:active state))
    (println "biggest" (:result state))
    (let [max-gen 10]
      (dotimes [y max-gen]
        (dotimes [x max-gen]
          (print (if-let [c (get field [x y])]
                   c
                   \space)))
        (println)))))


(defn day6-2 [in]
  (let [lines (into []
                    (map-indexed
                     (fn [idx line]
                       (let [[_ x y] (re-find #"(\d+), (\d+)" line)
                             [x y] (map #(Long/parseLong %) [x y])
                             id (if (<= idx 24)
                                  (char (+ (int \a) idx))
                                  (str "ID_" idx))]
                         [[x y] id])))
                    (xio/lines-in (string-in in)))
        max-x (apply max (map (comp first first) lines))
        max-y (apply max (map (comp second first) lines))

        result (reduce
                (fn [matching-cells-count [^long cx ^long cy]]
                  (let [sum (->> (for [[[^long x ^long y] id] lines]
                                   (+ (Math/abs (- cx x))
                                      (Math/abs (- cy y))))
                                 (reduce +))]
                    (if (< sum 10000)
                      (inc matching-cells-count)
                      matching-cells-count)))
                0
                (for [x (range (inc max-x))
                      y (range (inc max-y))]
                  [x y]))]
    result))

(comment
  (-> (day6-1 "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")
      print-state6)
  (day6-1 (io/resource "day6.txt"))



  (day6-2 "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

  (day6-2 (io/resource "day6.txt"))
  (:result *1)

  )

(defn vec-contains? [^clojure.lang.APersistentVector v x]
  (<= 0 (.indexOf v x)))

(defn day7-1 [in]
  (let [{:keys [child-parents parent-children]}
        (into {}
              (comp (map
                     (fn [line]
                       (let [[_ before _ _ _ _ _ after] (str/split line #" ")]
                         [before after])))
                    (x/transjuxt {:child-parents (comp (x/by-key second
                                                                 first
                                                                 (x/into #{}))
                                                       (x/into {}))
                                  :parent-children (comp (x/by-key first
                                                                   second
                                                                   (x/into #{}))
                                                         (x/into {}))}))
              (xio/lines-in (string-in in)))
        ;; root is node without parent, so does not appear in child
        parents (set (keys parent-children))
        children (set (keys child-parents))
        roots (reduce
               disj
               parents
               children)

        result (loop [fringe (into (sorted-set) roots)
                      out []]
                 (if-not (seq fringe)
                   (apply str out)
                   (let [expand (some
                                 (fn [node]
                                   (let [required-parents (get child-parents node)]
                                     (when (every?
                                            (fn [p]
                                              (vec-contains? out p))
                                            required-parents)
                                       node)))
                                 fringe)
                         children (get parent-children expand)]
                     (recur (-> fringe
                                (disj expand)
                                (into children))
                            (conj out expand)))))
        ]
    {:cp child-parents
     :pc parent-children
     :roots roots
     :result result}
    #_result))

(defn day7-2 [in work-length worker-count]
  (let [{:keys [child-parents parent-children]}
        (into {}
              (comp (map
                     (fn [line]
                       (let [[_ before _ _ _ _ _ after] (str/split line #" ")]
                         [before after])))
                    (x/transjuxt {:child-parents (comp (x/by-key second
                                                                 first
                                                                 (x/into #{}))
                                                       (x/into {}))
                                  :parent-children (comp (x/by-key first
                                                                   second
                                                                   (x/into #{}))
                                                         (x/into {}))}))
              (xio/lines-in (string-in in)))
        ;; root is node without parent, so does not appear in child
        parents (set (keys parent-children))
        children (set (keys child-parents))
        roots (reduce
               disj
               parents
               children)

        job-length (fn [job]
                     (+ work-length (- (int (first job)) 64)))

        workers (zipmap (into [:me]
                              (comp (map (fn [i] (keyword (str "elf-" i))))
                                    (take worker-count))
                              (range))
                        (repeat nil))
        
        result (loop [fringe (into (sorted-set) roots)
                      workers workers
                      seconds -1
                      out []]
                 (if (and (not (seq fringe))
                          (every? nil? (vals workers)))
                   {:seconds seconds
                    :out (apply str out)}
                   (let [done (for [[id [job todo]] workers
                                    :when (and job
                                               (= todo 0))]
                                job)
                         out (into out (apply sorted-set done))
                         fringe (into fringe
                                      (mapcat (fn [job]
                                                (get parent-children job)))
                                      done)
                         workers (into {}
                                       (for [[id [job todo]] workers]
                                         (if-not job
                                           [id nil]
                                           (if (= todo 0)
                                             [id nil]
                                             [id [job (dec todo)]]))))

                         available-workers (for [[id [job todo]] workers
                                                 :when (not job)]
                                             id)
                         assignments (->> (keep
                                           (fn [node]
                                             (let [required-parents (get child-parents node)]
                                               (when (every?
                                                      (fn [p]
                                                        (vec-contains? out p))
                                                      required-parents)
                                                 node)))
                                           fringe)
                                          (zipmap available-workers))]
                     (if (seq assignments)
                       (let [workers (reduce
                                      (fn [workers [id job]]
                                        (assoc workers id [job (dec (job-length job))]))
                                      workers
                                      assignments)
                             fringe (reduce
                                     (fn [fringe [id job]]
                                       (disj fringe job))
                                     fringe
                                     assignments)]
                         (recur fringe
                                workers
                                (inc seconds)
                                out))
                       (recur fringe
                              workers
                              (inc seconds)
                              out)))))
        ]
    {:cp child-parents
     :pc parent-children
     :roots roots
     :result result}
    #_result))

(comment
  (day7-1 "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
")
  (apply sorted-set "ABECD")

  (every? odd? nil)
  (vec-contains? (vec "ABCED") \E)
  (- (int \A) 64)
  
  (day7-1 (io/resource "day7.txt"))
  (str/split "Step C must be finished before step A can begin." #" ")

  (day7-2 "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
" 0 1)

  (day7-2 (io/resource "day7.txt") 60 4)


)

(defn day8-tree [in]
  (let [nums (->> (str/split in  #" ")
                  (mapv #(Long/parseLong %)))

        left (fn [at nums]
               ;; move from at to child-n: [... child-n meta-n {} {} {} at ...]
               (let [skip-children (->> (subvec nums 0 at)
                                        rseq
                                        (take-while map?)
                                        count)]
                 (- at skip-children
                    2 ;; to new child-n
                    )))

        tree (loop [at 0
                    nums nums]
               (if (map? (first nums))
                 (first nums)
                 (let [child-n (get nums at)
                       meta-n (get nums (+ at 1))
                       rest-nums (subvec nums (min (+ at 2)
                                                   (count nums)))
                       parsed-children-count (count (take-while map? rest-nums))]
                   (if (= parsed-children-count child-n)
                     (let [meta (into []
                                      (comp (drop child-n)
                                            (take meta-n))
                                      rest-nums)
                           children (into []
                                          (take child-n)
                                          rest-nums)
                           tail (drop (+ child-n meta-n) rest-nums)]
                       ;; in nums replace [child-n meta-n children+ meta...] with {:spec etc..}
                       (recur (long (left at nums))
                              (-> (subvec nums 0 at)
                                  (conj {:spec [child-n meta-n]
                                         :meta meta
                                         :children children})
                                  (into tail))))
                     (recur (+ at parsed-children-count 2)
                            nums)
                     ))))]
    tree))

(defn day8-1 [in]
  (let [tree (day8-tree in)
        meta-count (->> (tree-seq (comp seq :children) :children tree)
                        (mapcat :meta)
                        (reduce +))]
    {:tree tree
     :meta-count meta-count}))

(test/deftest day8-1-test
  (test/is (= (:tree (day8-1 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))
              ;;[2 3 [0 3 [] 10 11 12] [1 1 [0 1 [] 99] 2] 1 1 2]
              {:spec [2 3]
               :meta [1 1 2]
               :children [{:spec [0 3]
                           :meta [10 11 12]
                           :children []}
                          {:spec [1 1]
                           :meta [2]
                           :children [{:spec [0 1]
                                       :meta [99]
                                       :children []}]}]}
              )))

(defn day8-2 [in]
  (let [tree (day8-tree in)
        sum (walk/postwalk
             (fn [node]
               (if (map? node)
                 (let [{:keys [meta children]} node]
                   (if (seq children)
                     (->> (keep (fn [meta-entry]
                                  (when-let [child (get children (dec meta-entry))]
                                    child)) meta)
                          (reduce +))
                     (reduce + meta)))
                 node))
             tree)]
    sum))

(defn day8-tree-alt [input]
  (let [parse (fn parse [[child-n meta-n & more]]
                (let [[children more] #_(nth (iterate
                                              (fn [[children more]]
                                                (let [[child more] (parse more)]
                                                  [(conj children child)
                                                   more]))
                                              [[] more])
                                             child-n)
                      (reduce
                       (fn [[children more] _n]
                         (let [[child more] (parse more)]
                           [(conj children child)
                            more]))
                       [[] more]
                       (range child-n))
                      [meta more] (split-at meta-n more)]
                  [{:spec [child-n meta-n]
                    :meta meta
                    :children children} more]))]
    (first (parse input))))

(comment
  (day8-1-test)
  (day8-1 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
  (let [in (-> (slurp (io/resource "day8.txt"))
               str/split-lines
               first)]
    (time (day8-1 in)))


  (day8-2 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

  (let [in (-> (slurp (io/resource "day8.txt"))
               str/split-lines
               first)]
    (time (day8-2 in)))

  (day8-tree-alt [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

  )


(defn day9-1 [in]
  (let [[players last-marble] (->> (re-seq #"\d+" in)
                                   (map #(Long/parseLong %)))

        place-marble (fn [[high-scores ^java.util.ArrayList circle at] [id marble]]
                       (when (zero? (mod marble 10000))
                         (println "marble: " marble))
                       (if (zero? (mod marble 23))
                         (let [take-marble-idx (mod (- at 7) (.size circle))
                               take-marble (.get circle take-marble-idx)
                               new-circle (doto circle
                                            (.remove (int take-marble-idx)))

                               new-high-scores (update high-scores id (fnil + 0) marble take-marble)]
                           [new-high-scores
                            new-circle
                            take-marble-idx])
                         (let [new-at (inc (mod (+ at 1) (.size circle)))
                               new-circle (doto circle
                                            (.add new-at marble))]
                           [high-scores
                            new-circle
                            new-at])))

        [high-scores cicle at] (reduce
                                  place-marble
                                  [{}
                                   (doto (java.util.ArrayList.)
                                     (.add 0))
                                   0]
                                  (map list
                                       (cycle (map inc (range players)))
                                       (range 1 (inc last-marble))))
        [winner score] (apply max-key second high-scores)]
    score))


(defn day9-2 [in]
  (let [[players last-marble] (->> (re-seq #"\d+" in)
                                   (map #(Long/parseLong %)))

        circle (doto (java.util.ArrayList.)
                 (.add 0))
        high-scores (loop [marble 1
                           high-scores {}
                           at 0]
                      (if (= marble (inc last-marble))
                        high-scores
                        (let [id (inc (mod marble players))]
                          (if (zero? (mod marble 23))
                            (let [take-marble-idx (long (mod (- at 7) (.size circle)))
                                  take-marble (.get circle take-marble-idx)

                                  new-high-scores (update high-scores id (fnil + 0) marble take-marble)]
                              (doto circle
                                (.remove (int take-marble-idx)))
                              (recur (inc marble)
                                     new-high-scores
                                     take-marble-idx))
                            (let [new-at (long (inc (mod (+ at 1) (.size circle))))]
                              (doto circle
                                (.add new-at marble))
                              (recur (inc marble)
                                     high-scores
                                     new-at))))))
        [winner score] (apply max-key second high-scores)]
    score))

(defn logoot-compare [l r]
  (loop [i 0]
    (let [lx (get l i)
          rx (get r i)]
      (if (and (not lx)
               (not rx))
        0
        (if (not lx)
          -1
          (if (not rx)
            1
            (let [res (compare lx rx)]
              (if (zero? res)
                (recur (inc i))
                res))))))))


(defn next-index [sm index]
  (let [ks (-> (subseq sm > index)
               first)]
    (if ks
      (key ks)
      (key (first sm)))))

(defn prev-index [sm index]
  (let [ks (-> (rsubseq sm < index)
               first)]
    (if ks
      (key ks)
      (key (first (rseq sm))))))

(def QUARTER_LONG_MAX (long (/ Long/MAX_VALUE 4)))
(def HALF_LONG_MAX (long (/ Long/MAX_VALUE 2)))

(defn new-index [left-index right-index]
  (if (= left-index right-index)
    [HALF_LONG_MAX]
    (if (= (logoot-compare left-index right-index) 1)
      ;; left = last and right = first
      (conj left-index HALF_LONG_MAX)

      (loop [i 0
             common []]
        (let [lx (get left-index i)
              rx (get right-index i)]
          (if (= lx rx)
            (recur (inc i)
                   (conj common lx))
            (if (and (= lx 0)
                     (= rx 1))
              (conj left-index HALF_LONG_MAX)
              (conj common (long (Math/floor (/ (+ (or lx 0) rx) 2)))))))))))

(defn day9-2-alt [in]
  (let [[players last-marble] (->> (re-seq #"\d+" in)
                                   (map #(Long/parseLong %)))


        index [0]
        high-scores (loop [marble 1
                           index index
                           circle (sorted-map-by logoot-compare
                                                 index 0)
                           high-scores {}]
                      (when (zero? (mod marble 10000))
                        (println "marble" marble))
                      #_(do (println "================================")
                          (println "marble" marble)
                          (println "index" index)
                          (println "circle map" circle)
                          (println "circle" (vals circle)))
                      (if (= marble (inc last-marble))
                        high-scores
                        (let [id (inc (mod marble players))]
                          (if (zero? (mod marble 23))
                            (let [take-marble-idx (nth (iterate (partial prev-index circle) index) 7)
                                  take-marble (get circle take-marble-idx)

                                  index-new (next-index circle take-marble-idx)
                                  circle-new (dissoc circle take-marble-idx)
                                  ;;_ (println "marble " marble "take-marble" take-marble " idx "take-marble-idx "hmm" (keys circle))
                                  high-scores-new (update high-scores id (fnil + 0) marble take-marble)]
                              (recur (inc marble)
                                     index-new
                                     circle-new
                                     high-scores-new))
                            (let [index-neighbor (next-index circle index)
                                  index-neighbor-next (next-index circle index-neighbor)
                                  index-new (new-index index-neighbor index-neighbor-next)]
                              (recur (inc marble)
                                     index-new
                                     (assoc circle index-new marble)
                                     high-scores))))))
        [winner score] (apply max-key second high-scores)]
    score))

(defn day9-2-alt-2 [in]
  (let [[players last-marble] (->> (re-seq #"\d+" in)
                                   (map #(Long/parseLong %)))


        
        high-scores (loop [marble 1
                           index index
                           circle (sorted-map-by logoot-compare
                                                 index 0)
                           high-scores {}]
                      (when (zero? (mod marble 10000))
                        (println "marble" marble))
                      #_(do (println "================================")
                            (println "marble" marble)
                            (println "index" index)
                            (println "circle map" circle)
                            (println "circle" (vals circle)))
                      (if (= marble (inc last-marble))
                        high-scores
                        (let [id (inc (mod marble players))]
                          (if (zero? (mod marble 23))
                            (let [take-marble-idx (nth (iterate (partial prev-index circle) index) 7)
                                  take-marble (get circle take-marble-idx)

                                  index-new (next-index circle take-marble-idx)
                                  circle-new (dissoc circle take-marble-idx)
                                  ;;_ (println "marble " marble "take-marble" take-marble " idx "take-marble-idx "hmm" (keys circle))
                                  high-scores-new (update high-scores id (fnil + 0) marble take-marble)]
                              (recur (inc marble)
                                     index-new
                                     circle-new
                                     high-scores-new))
                            (let [index-neighbor (next-index circle index)
                                  index-neighbor-next (next-index circle index-neighbor)
                                  index-new (new-index index-neighbor index-neighbor-next)]
                              (recur (inc marble)
                                     index-new
                                     (assoc circle index-new marble)
                                     high-scores))))))
        [winner score] (apply max-key second high-scores)]
    score))

(test/deftest day9-1-test
  (test/are [in out] (= (day9-1 in) (day9-2 in)
                        (day9-2-alt in)

                        out
                        )
    "9 players; last marble is worth 25 points" 32
    "10 players; last marble is worth 1618 points" 8317
    "13 players; last marble is worth 7999 points" 146373
    "17 players; last marble is worth 1104 points" 2764
    "21 players; last marble is worth 6111 points" 54718
    "30 players; last marble is worth 5807 points" 37305
    ))





(comment
  (day9-1 "9 players; last marble is worth 25 points")

  (day9-1-test)

  (day9-1 (slurp (io/resource "day9.txt")))
  (day9-2 (slurp (io/resource "day9.txt")))

  (day9-2 (-> (slurp (io/resource "day9.txt"))
              (str/replace " points" "00 points")))


  (sort logoot-compare [[5] [4 8] [4]])

  (-> (sorted-map-by logoot-compare
                     [5] 3 [4 8] 2 [4] 1
                     )
      #_(keys)
      (subseq >= [4])
      ;;first
      ;;key
      )

  (logoot-compare [3] [3 8])
  
  (-> (sorted-map-by logoot-compare
                     [5] 3 [4 8] 2 [4] 1
                     )
      (next-index [4 8])
      #_(prev-index [4]))

  (let [sm (sorted-map-by logoot-compare
                          [5] 3 [4 8] 2 [4] 1
                          )
        ]
    (nth (iterate (partial prev-index sm) [4 8]) 4))

  (let [sm (sorted-map-by logoot-compare
                          [5] 3 [4 8] 2 [4] 1
                          )
        ]
    (first  (keys sm))
    (rsubseq sm > [4 8]))

  (new-index [0] [0 1])
  (sorted-map-by logoot-compare
                 [1] 3 [0 1] 2 [0] 1

                 )

  (day9-2-alt "9 players; last marble is worth 25 points")

  (day9-2-alt (slurp (io/resource "day9.txt")))

  (day9-2-alt (-> (slurp (io/resource "day9.txt"))
                  (str/replace " points" "00 points")))

  )
