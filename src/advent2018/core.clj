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
