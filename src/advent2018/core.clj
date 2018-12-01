(ns advent2018.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
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
