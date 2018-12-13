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
  
  


(defn power [x y serial]
  (let [rack-id (+ x 10)
        power-level-start (* rack-id y)
        power-level-next (+ power-level-start serial)
        power-level-mult (* power-level-next rack-id)
        hundred (-> (/ power-level-mult 100)
                    long
                    (mod 10))
        minus (- hundred 5)]
    minus))

(defn day11-1 [serial]
  (let [result (loop [x 1
                      y 1
                      max-power Long/MIN_VALUE
                      max-coor nil]
                 (if (and (= x 298)
                          (= y 298))
                   max-coor
                   (let [points (for [dx (range 3)
                                      dy (range 3)]
                                  (let [[sx sy] [(+ x dx) (+ y dy)]]
                                    (power sx sy serial)))
                         power (reduce + points)
                         next-x (let [x (inc x)]
                                  (if (= x 301)
                                    1
                                    x))
                         next-y (if (= x 300)
                                  (inc y)
                                  y)]
                     (if (< max-power power)
                       (recur next-x
                              next-y
                              power
                              [x y])
                       (recur next-x
                              next-y
                              max-power
                              max-coor))))
                 )
        [x y] result]
    (str x "," y)))

(defn day11-2 [serial]
  (let [sum-table (reduce
                   (fn [sum-table [x y]]
                     (let [i (power x y serial)
                           i-x (get sum-table [(dec x) y] 0)
                           i-y (get sum-table [x (dec y)] 0)
                           i-xy (get sum-table [(dec x) (dec y)] 0)
                           sum (- (+ i i-x i-y) i-xy)]
                       (assoc sum-table [x y] sum)))
                   {}
                   (for [x (range 1 301)
                         y (range 1 301)]
                     [x y]))
        winner (apply
                max-key
                (fn [[x y size power]]
                  power)
                (for [size (range 1 301)
                      x (range 1 (- 301 size))
                      y (range 1 (- 301 size))]
                  (let [a (get sum-table [x y])
                        b (get sum-table [(+ x size) y])
                        c (get sum-table [x (+ y size)])
                        d (get sum-table [(+ x size) (+ y size)])]
                    [x y size (- (+ a d)
                                 b c)])))
        [x y size power] winner]
    (str (inc x) "," (inc y) "," size)))

(test/deftest day11-1-test
  (test/is
   (= (power 3 5 8) 4))
  (test/are [x y serial out] (= (power x y serial) out)
    122 79  57 -5
    217 196 39 0
    101 153 71 4
    )
  (test/are [serial out] (= (day11-1 serial) out)
    18 [33 45]
    42 [21 61]))

(test/deftest day11-2-test
  (test/are [serial out] (= (day11-2 serial) out)
    18 "90,269,16"
    42 "232,251,12"))


(comment
  (day11-1-test)

  (day11-1 7989) 19,17

  (day11-2-test)
  (day11-2 7989)

  )

(defn day12-1 [in]
  (let [lines (into [] (xio/lines-in (core/string-in in)))
        [init _blank & rules] lines
        init [0 (vec (subs init (count "initial state: ")))]
        rules (into {}
                    (map (fn [rule]
                           (let [clause (vec (subs rule 0 5))
                                 effect (first (subs rule 9 10))]
                             [clause effect])))
                    rules)

        I (atom -1)
        iters (iterate
               (fn [[offset pots]]
                 (let [i (swap! I inc)
                       offset-out (- offset 2)]
                   (when (<= i 20)
                     (println "pots" (let [ostr (pr-str [i offset offset-out])]
                                       (str (apply str (repeat (- 10 (count ostr)) " "))
                                            " "
                                            ostr))
                              "|"
                              (apply str (repeat (- 10 (* -1 offset)) " "))
                              (apply str pots)))
                   (let [
                         pots-out (->> (concat [\. \. \. \.]
                                               pots
                                               [\. \. \. \.])
                                       (partition 5 1)
                                       (mapv #(get rules % \.)))
                         empty-prefix (count (take-while #{\.} pots-out))
                         empty-suffix (count (take-while #{\.} (rseq pots-out)))
                         new-offset-out (+ offset-out empty-prefix)
                         pots-out (subvec pots-out empty-prefix
                                          (- (count pots-out) empty-suffix))]
                     ;;(println "offset" offset "offset-out" offset-out "new-offset-out" new-offset-out "empty prefix" empty-prefix "empty suffix" empty-suffix)
                     ;;(println "pots-out" (apply str pots-out))
                     [new-offset-out pots-out])))
               init)

        [offset pots] (nth iters 20)
        result (->> (map (fn [idx pot]
                           (if (= pot \#)
                             idx
                             0))
                         (range offset (+ offset (count pots)))
                         pots)
                    (reduce +))]
    {:init init
     :rules rules
     ;;:iters iters
     :result result}))

(defn day12-2 [in]
  (let [lines (into [] (xio/lines-in (core/string-in in)))
        [init _blank & rules] lines
        init (vec (subs init (count "initial state: ")))
        rules (into {}
                    (map (fn [rule]
                           (let [clause (vec (subs rule 0 5))
                                 effect (first (subs rule 9 10))]
                             [clause effect])))
                    rules)

        [offset pots] (loop [i 0
                             offset 0
                             pots init
                             seen {}]
                        (if (= i
                               50000000000
                               #_20)
                          [offset pots]
                          (let [pots-out (->> (concat [\. \. \. \.]
                                                      pots
                                                      [\. \. \. \.])
                                              (partition 5 1)
                                              (mapv #(get rules % \.)))
                                empty-prefix (count (take-while #{\.} pots-out))
                                empty-suffix (count (take-while #{\.} (rseq pots-out)))
                                new-offset (+ offset -2 empty-prefix)
                                pots-out (subvec pots-out empty-prefix
                                                 (- (count pots-out) empty-suffix))]
                            (if-let [[prev-id prev-offset] (get seen pots-out)]
                              (let [offset-diff (- offset prev-offset)
                                    loop-length (- i prev-id)
                                    togo (- 50000000000 i)
                                    last-part (mod togo loop-length)
                                    final-idx (+ prev-id last-part)
                                    offset (+ offset (* offset-diff togo))]
                                [offset pots-out])
                              (recur (+ i 1)
                                     new-offset
                                     pots-out
                                     (assoc seen pots [i offset]))))))

        result (->> (map (fn [idx pot]
                           (if (= pot \#)
                             idx
                             0))
                         (range offset (+ offset (count pots)))
                         pots)
                    (reduce +))]
    {:init init
     :rules rules
     ;;:iters iters
     :result result}))

(comment
  (day12-1
"initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #"
)

  (day12-1 (io/resource "day12.txt"))
  (-> (day12-2 (io/resource "day12.txt"))
      :result)
  
  (subvec (vec "ABCDE") 1)
  (range (- 0 2) 5)



  )
