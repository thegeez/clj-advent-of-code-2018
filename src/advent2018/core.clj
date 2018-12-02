(ns advent2018.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.test :as test]
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
  (day2-2 (io/resource
            "day2.edn"
            ;;"day2_test2.edn"
            ))

  (day2-2-test)
  )
