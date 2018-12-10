(ns advent2018.core2
  (:require [advent2018.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]
            [net.cgrand.xforms.rfs :as xrf]))

(defn day10-1 [in]
  (let [points (into []
                     (map (fn [line]
                            (let [[px py vx vy] (->> (re-seq #"-?\d+" line)
                                                     (map #(Long/parseLong %)))]
                              [[px py] [vx vy]])))
                     (xio/lines-in (core/string-in in)))

        
        
        get-px (fn [[[px py] vel]]
                 px)
        get-py (fn [[[px py] vel]]
                 py)
        calc-bounding-box (x/transjuxt
                           {:top-left (x/transjuxt
                                       {:x (comp (map get-px)
                                                 x/min)
                                        :y (comp (map get-py)
                                                 x/min)})
                            :bottom-right (x/transjuxt
                                           {:x (comp (map get-px)
                                                     x/max)
                                            :y (comp (map get-py)
                                                     x/max)})})

        tick (fn [{:keys [positions _bounding-box iter]}]
               (into {:iter (inc (or iter 0))}
                     (comp (map (fn [[[px py] [vx vy]]]
                                  [[(+ px vx)
                                    (+ py vy)]
                                   [vx vy]]))
                           (x/transjuxt
                            {:positions (x/into [])
                             :pixels (comp (map (fn [[pxy vel]]
                                                  pxy))
                                           (x/into #{}))
                             :bounding-box calc-bounding-box}))
                     positions))
        
        result (->> (iterate tick {:positions points})
                    rest
                    (map (fn [result]
                           (println "iter" (:iter result))
                           (let [bounding-box (:bounding-box result)
                                 {tlx :x, tly :y} (:top-left bounding-box)
                                 {brx :x, bry :y} (:bottom-right bounding-box)
                                 min-box (+ (- brx tlx)
                                            (- bry tly))]
                             (assoc result :min-box min-box))))
                    #_(take 7)
                    (partition 2 1)
                    (some (fn [[l r]]
                            (when (< (:min-box l) (:min-box r))
                              l)))
                    (conj []))]
    (doseq [r result]
      (println "===============================")
      (println "bounding box" (pr-str (:bounding-box r)))
      (println "min-box" (:min-box r))
      (println "iter" (:iter r))
      (let [bb (:bounding-box r)
            tlx (get-in bb [:top-left :x])
            tly (get-in bb [:top-left :y])
            brx (get-in bb [:bottom-right :x])
            bry (get-in bb [:bottom-right :y])
            ]
        (doseq [y (range tly (inc bry))]
          (doseq [x (range tlx (inc brx))]
            (print (if (get (:pixels r) [x y])
                     \#
                     \.)))
          (println))))
    result))

(comment
  (day10-1 "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")
  


  (day10-1 (io/resource "day10.txt"))

  )
  
  
