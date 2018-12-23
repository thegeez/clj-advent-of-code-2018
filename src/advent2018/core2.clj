(ns advent2018.core2
  (:require [advent2018.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
            [clojure.walk :as walk]
            [clojure.zip :as zip]
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
                         power (long (reduce + points))
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

(defn day13-1 [in]
  (let [lines (into []
                    (map vec)
                    (xio/lines-in (core/string-in in)))

        empty-carts (sorted-map-by (fn [[lx ly] [rx ry]]
                                     ;; rows prio over cols
                                     (compare [ly lx] [ry rx])))
        [track carts] (reduce
                       (fn [[track carts] [x y]]
                         (let [c (get-in track [y x])
                               cart (get (set "<>v^") c)
                               cart-replace (zipmap "<>v^" "--||")]
                           (if cart
                             [(assoc-in track [y x] (get cart-replace cart))
                              (assoc carts [x y] [cart :left])]
                             [track carts])))
                       [lines empty-carts]
                       (for [x (range (count (first lines)))
                             y (range (count lines))]
                         [x y]))

        tick (fn [carts]
               (reduce
                (fn [carts [[x y] [c crossing]]]
                  (let [[dx dy] (get {\> [1 0]
                                      \< [-1 0]
                                      \^ [0 -1]
                                      \v [0 1]} c)
                        [nx ny] (map + [x y] [dx dy])
                        t (get-in track [ny nx])

                        [nc ncrossing] (cond
                                         (= t \\)
                                         [(get (zipmap "><^v" "v^<>") c) crossing]
                                         (= t \/)
                                         [(get (zipmap "><^v" "^v><") c) crossing]
                                         (= t \+)
                                         (let [nc (get (get {:left (zipmap "><^v" "^v<>")
                                                             :right (zipmap "><^v" "v^><")
                                                             :straight (zipmap "><^v" "><^v")} crossing) c)
                                               ncrossing (get {:left :straight
                                                               :straight :right
                                                               :right :left} crossing)]
                                           [nc ncrossing])
                                         :else
                                         [c crossing])]
                    (if (contains? carts [nx ny])
                      (reduced (reduced [nx ny]))
                      (-> carts
                          (dissoc [x y])
                          (assoc [nx ny] [nc ncrossing])))))
                carts
                carts))

        [x y :as crash-site]
        (->> (iterate tick carts)
             ;;(drop 40) (take 20)
             ;;(take 20)
             #_(mapv (fn [carts]
                       (when (not (reduced? carts))
                         (do (println "carts" carts)
                             (doseq [y (range (count track))]
                               (doseq [x (range (count (first track)))]
                                 (print (if-let [[c crossing] (get carts [x y])]
                                          c
                                          (get-in track [y x]))))
                               (println))
                             (println "===========================================")))
                       carts))
             #_(map-indexed (fn [idx carts]
                            (when (not (reduced? carts))
                              (spit "run.html"
                                    (with-out-str
                                      (do
                                        (println (str"<a name=\"step" idx "\">" idx "</a><br>"
                                                     "<a href=\"#step" (dec idx) "\">prev</a><br>"
                                                     "<a href=\"#step" (inc idx) "\">next</a>"))
                                        
                                        (println "<br>" carts "<br>")
                                        (doseq [y (range 20 (count track))]
                                          (doseq [x (range (count (first track)))]
                                            (print (if (or (= [x y] [134 96])
                                                           (= [x y] [43 91]))
                                                     "<span style=\"color: red;\"><b>X</b></span>"
                                                       (if-let [[c crossing] (get carts [x y])]
                                                         (str "<span style=\"color: " (get {:left "red"
                                                                                            :right "blue"
                                                                                            :straight "green"} crossing) ";\"><b>" c "</b></span>")
                                                         (let [t (get-in track [y x])]
                                                           (if (= t \space)
                                                             "&nbsp;"
                                                             t))))))
                                          (println "<br>"))
                                        (println "<br><br>")))
                                    :append true))
                            carts))
             (some (fn [res]
                     (when (reduced? res)
                       (unreduced res)))))
        ]
    (str x "," y)))

(defn day13-2 [in]
  (let [lines (into []
                    (map vec)
                    (xio/lines-in (core/string-in in)))

        empty-carts (sorted-map-by (fn [[lx ly] [rx ry]]
                                     ;; rows prio over cols
                                     (compare [ly lx] [ry rx])))
        [track carts] (reduce
                       (fn [[track carts] [x y]]
                         (let [c (get-in track [y x])
                               cart (get (set "<>v^") c)
                               cart-replace (zipmap "<>v^" "--||")]
                           (if cart
                             [(assoc-in track [y x] (get cart-replace cart))
                              (assoc carts [x y] [cart :left])]
                             [track carts])))
                       [lines empty-carts]
                       (for [x (range (count (first lines)))
                             y (range (count lines))]
                         [x y]))

        ;;_ (println "carts" carts)
        tick (fn [carts]
               (->> (iterate
                     (fn [[new-carts old-carts]]
                       (if-not (first old-carts)
                         (reduced new-carts)
                         (let [[[x y] [c crossing]] (first old-carts)
                               [dx dy] (get {\> [1 0]
                                             \< [-1 0]
                                             \^ [0 -1]
                                             \v [0 1]} c)
                               [nx ny] (map + [x y] [dx dy])
                               t (get-in track [ny nx])

                               [nc ncrossing] (cond
                                                (= t \\)
                                                [(get (zipmap "><^v" "v^<>") c) crossing]
                                                (= t \/)
                                                [(get (zipmap "><^v" "^v><") c) crossing]
                                                (= t \+)
                                                (let [nc (get (get {:left (zipmap "><^v" "^v<>")
                                                                    :right (zipmap "><^v" "v^><")
                                                                    :straight (zipmap "><^v" "><^v")} crossing) c)
                                                      ncrossing (get {:left :straight
                                                                      :straight :right
                                                                      :right :left} crossing)]
                                                  [nc ncrossing])
                                                :else
                                                [c crossing])]
                           (if (contains? new-carts [nx ny])
                             [(dissoc new-carts [nx ny])
                              (dissoc old-carts [x y])]
                             (if (contains? old-carts [nx ny])
                               [new-carts
                                (-> old-carts
                                    (dissoc [nx ny])
                                    (dissoc [x y]))]
                               [(assoc new-carts [nx ny] [nc ncrossing])
                                (dissoc old-carts [x y])])))))
                     [empty-carts carts])
                    (some (fn [carts]
                            (when (reduced? carts)
                              (unreduced carts))))))

        [x y :as crash-site]
        (->> (iterate tick carts)
             (some (fn [carts]
                     (when (= (count carts) 1)
                       (ffirst carts)))))
        ]
    (str x "," y)))


(comment
  (day13-1 "/->-\\        
|   |  /----\\
| /-+--+-\\  |
| | |  | v  |
\\-+-/  \\-+--/
  \\------/      ")

  (day13-1 (io/resource "day13.txt"))

  (-> (sorted-map-by (fn [[lx ly] [rx ry]]
                       (compare [ly lx] [ry rx])) [8 2] :a [6 2] :b)
      first)

  (day13-2 "/>-<\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/")

  (day13-2 "/><-\\  
|   |  
| /<+-\\
| | | v
\\>+</ |
  |   ^
  \\<->/")

  (day13-2 (io/resource "day13.txt"))

  )


(defn day14-1 [in]
  (let [step (fn [{:keys [board at]}]
               (let [elf-0-r (get board (val (first at)))
                     elf-1-r (get board (val (second at)))
                     append-num (+ elf-0-r
                                   elf-1-r)
                     add-nums (map #(Long/parseLong (str %)) (pr-str append-num))
                     board (into board
                                 add-nums)
                     at (zipmap (keys at)
                                (map (fn [idx]
                                       (let [b (get board idx)]
                                         (mod (+ idx b 1) (count board)))) (vals at)))]
                 {:board board
                  :at at}))

        res (->> (iterate step {:board [3 7]
                                :at {0 0
                                     1 1}})
                 #_(take 16)
                 #_(map (fn [{:keys [board at done] :as res}]
                          ;;(println "done" done (count (into (get done 0) (get done 1))))
                        (doseq [[idx b] (map list (range) board)]
                          (if-let [[eo ec] (some (fn [[elf at]]
                                                   (when (= at idx)
                                                     (get {0 (seq "()")
                                                           1 (seq "[]")} elf)))
                                                 at)]
                            (print (str eo b ec))
                            (print (str " " b " "))))
                        (println)
                        res))
                 (some (fn [{:keys [board] :as res}]
                         (when (<= (+ in 10) (count board))
                           (assoc res :result (apply str
                                                     (->> board
                                                          (drop in)
                                                          (take 10))))))))]
    res))

(set! *warn-on-reflection* true)

(test/deftest day14-1-test
  (test/are [in out] (= (:result (day14-1 in)) out)
    9 "5158916779"
    5 "0124515891"
    18 "9251071085"
    2018 "5941429882"
    ))

(defn day14-2 [in]
  (let [num->nums (fn [n]
                    (map #(Long/parseLong (str %)) (pr-str n)))
        
        step (fn [{:keys [board at iter]}]
               (let [elf-0-r (get board (val (first at)))
                     elf-1-r (get board (val (second at)))
                     append-num (+ elf-0-r
                                   elf-1-r)
                     add-nums (num->nums append-num)
                     old-board-count (count board)
                     board (into board
                                 add-nums)
                     at (zipmap (keys at)
                                (map (fn [idx]
                                       (let [b (get board idx)]
                                         (mod (+ idx b 1) (count board)))) (vals at)))]
                 {:board board
                  :at at
                  :prev-count old-board-count
                  :iter (inc iter)}))

        needle (map #(Long/parseLong (str %)) in)
        needle-count (count needle)
        
        res (->> (iterate step {:board [3 7]
                                :at {0 0
                                     1 1}
                                :prev-count 0
                                :iter 0})
                 ;;(take 40)
                 #_(map (fn [{:keys [board at done] :as res}]
                          ;;(println "done" done (count (into (get done 0) (get done 1))))
                          (doseq [[idx b] (map list (range) board)]
                            (if-let [[eo ec] (some (fn [[elf at]]
                                                     (when (= at idx)
                                                       (get {0 (seq "()")
                                                             1 (seq "[]")} elf)))
                                                   at)]
                              (print (str eo b ec))
                              (print (str " " b " "))))
                          (println)
                          res))
                 #_(take 30000)
                 #_(map (fn [res]
                          (when (zero? (mod (:iter res) 1000))
                            (let [{:keys [board at]} res]
                              (doseq [[idx b] (map list (range) board)]
                                (if-let [[eo ec] (some (fn [[elf at]]
                                                         (when false #_(= at idx)
                                                               (get {0 (seq "()")
                                                                     1 (seq "[]")} elf)))
                                                       at)]
                                  (print (str eo b ec))
                                  (print (str " " b " ")))))
                            (println)
                            )
                          res))
                 (some (fn [{:keys [board prev-count] :as res}]
                         (let [match-at (if (= (subvec board (max 0 (- (count board) needle-count)))
                                               needle)
                                          (- (count board) needle-count)
                                          (if (= (subvec board (max 0 (- (count board) needle-count 1)))
                                                 needle)
                                            (- (count board) needle-count 1)
                                            nil))]
                           (when match-at
                             (assoc res :result match-at))))))]
    res))

(defn day14-2-alt [in]
  (let [needle (mapv #(Long/parseLong (str %)) in)

        match (if (= (count needle) 6)
                (let [[na nb nc nd ne nf] needle]
                  (fn [[a b c d e f]]
                    (and (= a na)
                         (= b nb)
                         (= c nc)
                         (= d nd)
                         (= e ne)
                         (= f nf))))
                (let [[na nb nc nd ne] needle]
                  (fn [[a b c d e]]
                    (and (= a na)
                         (= b nb)
                         (= c nc)
                         (= d nd)
                         (= e ne)))))

        res (loop [i 2
                   board [3 7]
                   at-0 0
                   at-1 1]
              (let [rep-0 (get board at-0)
                    rep-1 (get board at-1)
                    add-nums (+ rep-0 rep-1)
                    tens (let [t (quot add-nums 10)]
                           (when-not (zero? t)
                             t))
                    ones (mod add-nums 10)
                    board (cond-> board
                            tens
                            (conj tens)
                            true
                            (conj ones))
                    at-end-one-prev (- (count board)
                                       (if (= (count needle) 6)
                                         7
                                         6))
                    done (if (and
                              (< 7 i)
                              tens
                              (match (subvec board at-end-one-prev (count board))))
                           at-end-one-prev
                           (let [at-end (- (count board)
                                           (if (= 6 (count needle))
                                             6
                                             5))]
                             (if (and (< 6 i)
                                      (match (subvec board at-end (count board))))
                               at-end
                               nil)))]
                (or done
                    (recur (inc i)
                           board
                           (long (mod (+ at-0 rep-0 1) (count board)))
                           (long (mod (+ at-1 rep-1 1) (count board)))))))]
    res))

(test/deftest day14-2-test
  (test/are [in out] (= (:result (day14-2 in))
                        out)
    "515891" 9
    "012451" 5
    "92510" 18
    "59414" 2018
    ))

(test/deftest day14-2-alt-test
  (test/are [in out] (= (day14-2-alt in)
                        out)
    "515891" 9
    "51589" 9
    "012451" 5
    "92510" 18
    "59414" 2018
    ))



(comment
  (day14-1 9)
  (day14-1-test)
  (day14-1 147061)

  (day14-2-test)

  (day14-2-alt-test)

  (day14-2-alt "147061")

  (long (/ 6 10))
  (quot 01 10)
  )

(defn print-round [{:keys [turn width height world goblins elfs]}]
  (println turn ;;"after" turn "rounds"
           )
  (doseq [y (range height)]
    (let [res (map
               (fn [x]
                 (let [w (get world [x y])
                       g (get goblins [x y])
                       e (get elfs [x y])]
                   [(cond
                      w \#
                      g \G
                      e \E
                      true \.)
                    (cond
                      g (str "G(" g ")")
                      e (str "E(" e ")"))]))
               (range width))]
      (println (apply str (map first res)) (str/join " " (keep second res)))))
  (println))

(def world-map-compare (fn [[lx ly] [rx ry]]
                         (compare [ly lx] [ry rx])))
(def world-map (sorted-map-by world-map-compare))

(defn combat [[x y] defend damage]
  (let [res (if-let [[attack-xy health :as attack] (->> (keep
                                                         (fn [[dx dy]]
                                                           (find defend [(+ x dx) (+ y dy)]))
                                                         [[0 -1]
                                                          [-1 0]
                                                          [1 0]
                                                          [0 1]])
                                                        ;; attack opponent with min health
                                                        (sort-by second)
                                                        first)]
              (let [health-new (- health damage)
                    defend (if (< 0 health-new)
                             (assoc defend attack-xy health-new)
                             (dissoc defend attack-xy))]
                defend)
              defend)]
    #_(when (#{11 12 13} y)
      (println "combat" [x y] defend "res" res "action?" (not= defend res)))
    res))


(defn move [[x y] world from to]
  #_(println "move?" [x y])
  (let [options (for [[dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                      :let [[cx cy] [(+ x dx) (+ y dy)]]
                      :when (and (not (contains? world [cx cy]))
                                 (not (contains? from [cx cy]))
                                 (not (contains? to [cx cy])))]
                  [cx cy])]
    (if-let [options (seq options)]
      (let [enemy-candidates (into (sorted-set-by (fn [[lx ly ldist]
                                                       [rx ry rdist]]
                                                    (compare [ldist ly lx]
                                                             [rdist ry rx])))
                                   (for [[[tx ty] _health] to
                                         [dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                                         :let [[cx cy] [(+ tx dx) (+ ty dy)]]
                                         :when (and (not (contains? world [cx cy]))
                                                    (or (= [cx cy] [x y]) ;; do consider staying in the same place
                                                        (not (contains? from [cx cy])))
                                                    (not (contains? to [cx cy])))]
                                     (let [dist (+ (Math/abs ^long (-' x cx))
                                                   (Math/abs ^long (-' y cy)))]
                                       [cx cy dist])))]
        #_(println "enemy-candidates" enemy-candidates)
        (if (when-let [[_x _y min-dist] (first enemy-candidates)]
              (zero? min-dist))
          ;; we are next to a target, so don't move
          (do
            #_(println "not moving, next to target" [x y])
            nil)
          (let [
                ;; active + seen mapping square -> origin with shortest path to here
                res (loop [active (merge (sorted-map-by world-map-compare)
                                         (zipmap options
                                                 options))
                           seen {}]
                      #_(println "active" active)
                      (if-let [match (some (fn [xy]
                                             (when-let [origin (get active xy)]
                                               origin))
                                           enemy-candidates)]
                        (do
                          #_(println "do move" [x y] "->" match)
                          match)
                        (let [prefer-origin-read-order (fn [seen [to origin]]
                                                         (if-let [o (get seen to)]
                                                           (if (= -1 (world-map-compare origin o))
                                                             (assoc seen to origin)
                                                             seen)
                                                           (assoc seen to origin)))
                              seen (reduce
                                    prefer-origin-read-order
                                    seen
                                    active)
                              active (reduce
                                      prefer-origin-read-order
                                      (empty active)
                                      (for [[[x y] origin] active
                                            [dx dy] [[0 -1] [-1 0] [1 0] [0 1]]
                                            :let [[cx cy] [(+ x dx) (+ y dy)]
                                                  #_ (println "expand" txy [cx cy])]
                                            :when (and (not (contains? seen [cx cy]))
                                                       (not (contains? world [cx cy]))
                                                       (not (contains? from [cx cy]))
                                                       (not (contains? to [cx cy])))]
                                        (do #_(println "expand keep" txy [cx cy])
                                            [[cx cy] origin])))]
                          (if-not (seq active)
                            nil
                            (recur active
                                   seen)))))]
            res)))
      ;; [x y] is blocked in, therefore go nowhere
      nil)))

(defn tick-round [{:keys [turn width height world goblins elfs damage-elf] :as round
                   :or {damage-elf 3}}]
  (let [turn-order (into (sorted-map-by world-map-compare)
                         (concat (map (fn [xy] [xy :G]) (keys goblins))
                                 (map (fn [xy] [xy :E]) (keys elfs))))
        do-tick (fn [turn-xy world attack defend turn-type]
                  (let [damage (if (= turn-type :G)
                                 3
                                 damage-elf)]
                    (if-let [turn-xy-new (move turn-xy world attack defend)]
                      (let [health (get attack turn-xy)
                            attack-new (-> attack
                                           (dissoc turn-xy)
                                           (assoc turn-xy-new health))]
                        [attack-new
                         (combat turn-xy-new defend damage)])
                      [attack
                       (combat turn-xy defend damage)])))
        [goblins elfs done] (reduce
                             (fn [[goblins elfs :as ge] [turn-xy turn-type]]
                               ;;(println "turn-xy" turn-xy (get goblins turn-xy))
                               (if (or (not (seq goblins))
                                       (not (seq elfs)))
                                 (reduced [goblins elfs :done])
                                 (if-let [g (and (= turn-type :G)
                                                 (get goblins turn-xy))]
                                   (do-tick turn-xy world goblins elfs turn-type)
                                   (if-let [e (and (= turn-type :E)
                                                   (get elfs turn-xy))]
                                     (let [[e g] (do-tick turn-xy world elfs goblins turn-type)]
                                       [g e])
                                     ;; planned turn died before being its turn
                                     ge))))
                             [goblins elfs]
                             turn-order)]
    {:turn (inc turn)
     :done done
     :width width
     :height height
     :world world
     :goblins goblins
     :elfs elfs
     :damage-elf damage-elf}))

(defn day15-1 [in]
  (let [lines (vec (str/split-lines in))
        width (count (take-while #{\#} (first lines)))
        height (count lines)
        all (into world-map
                  (for [[y line] (map-indexed list lines)
                        [x c] (map-indexed list (take width line))

                        :when (contains? #{\G \E \#} c)]
                    [[x y] c]))
        world (into world-map
                    (for [[xy c] all
                          :when (= c \#)]
                      [xy c]))
        goblins (into world-map
                      (for [[xy c] all
                            :when (= c \G)]
                        [xy 200]))
        elfs (into world-map
                   (for [[xy c] all
                         :when (= c \E)]
                     [xy 200]))
        round {:turn 0
               :width width
               :height height
               :world world
               :goblins goblins
               :elfs elfs}

        _ (print-round round)
        res (->> (iterate (fn [round]
                            (let [res (tick-round round)]
                              (when (#{0 1 2 63 64 65
                                       ;;79 80 81
                                       } (:turn res))
                                    (print-round res))
                              res))
                          round)
                 #_(take 4)
                 (some (fn [round]
                         (when (:done round)
                           (let [last-round (dec (:turn round))
                                 score (+ (reduce + (vals (:goblins round)))
                                          (reduce + (vals (:elfs round))))
                                 result (* last-round score)]
                             (println "result" result "score" score "last-round" last-round)
                             (assoc round :result result))))))]
    res))

(test/deftest day15-1-test
  (test/are [in out] (= (:result (day15-1 in)) out)
    "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######" 27730

"#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######" 36334

"#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######" 27755

"#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######" 28944

"#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########" 18740

))


(defn day15-2 [in]
  (let [lines (vec (str/split-lines in))
        width (count (take-while #{\#} (first lines)))
        height (count lines)
        all (into world-map
                  (for [[y line] (map-indexed list lines)
                        [x c] (map-indexed list (take width line))

                        :when (contains? #{\G \E \#} c)]
                    [[x y] c]))
        world (into world-map
                    (for [[xy c] all
                          :when (= c \#)]
                      [xy c]))
        goblins (into world-map
                      (for [[xy c] all
                            :when (= c \G)]
                        [xy 200]))
        elfs (into world-map
                   (for [[xy c] all
                         :when (= c \E)]
                     [xy 200]))
        round {:turn 0
               :width width
               :height height
               :world world
               :goblins goblins
               :elfs elfs}

        res (some
             (fn [damage-elf]
               #_(println "damage-elf" damage-elf)
               (let [round (assoc round :damage-elf damage-elf)
                     attempt (->> (iterate (fn [round]
                                             (let [round (tick-round round)]
                                               ;;(print-round round)
                                               #_(when (= damage-elf 15)
                                                 (println "damage-elfs" damage-elf (count (:elfs round)) (count elfs)))
                                               (if (= (count (:elfs round)) (count elfs))
                                                 round
                                                 (assoc round :elf-dead true))))
                                           round)
                                  #_(take 4)
                                  (some (fn [round]
                                          ;;(println "turn" (:turn round))
                                          (if (:elf-dead round)
                                            :failed
                                            (when (:done round)
                                              (let [last-round (dec (:turn round))
                                                    score (+ (reduce + (vals (:goblins round)))
                                                             (reduce + (vals (:elfs round))))
                                                    result (* last-round score)]
                                                (println "result" result "score" score "last-round" last-round "damage-elf" damage-elf)
                                                (assoc round :result result)))))))]
                 (when (not= :failed attempt)
                   attempt)))
             (map #(+ % 4) (range)))
        ;;_ (print-round round)
        ]
    res))


(test/deftest day15-2-test
  (test/are [in out] (= (:result (day15-2 in)) out)
    "#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######" 4988

    "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######" 31284

    "#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######" 3478

    "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######" 6474

    "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########" 1140))


(comment

  (day15-1 "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")


  (day15-1
   "#######   
#.G...#   G(200)
#...EG#   E(200), G(200)
#.#.#G#   G(200)
#..G#E#   G(200), E(200)
#.....#
#######"   
   )

  (-> (day15-1
       "#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######")
      :result)
  ;; round 37 36334


  (day15-1 "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######")

  (day15-1 "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")

  
  (day15-1-test)


  (day15-2-test)

  (day15-2 (slurp (io/resource "day15.txt")))
  )


(defn split-day16 [in]
  (let [lines (into []
                    (xio/lines-in (core/string-in in)))
        [part1 part2] (->> [[] nil lines]
                           (iterate (fn [[p1 p2 lines]]
                                      (let [line (first lines)]
                                        (if (str/starts-with? line "Before")
                                          (let [[before op after _blank & lines] lines
                                                example {:before (read-string (subs before (count "Before: ")))
                                                         :op (mapv #(Long/parseLong %) (str/split op #" "))
                                                         :after (read-string (subs after (count "After: ")))}]
                                            [(conj p1 example) p2 lines])
                                          (let [program (->> lines
                                                             (drop-while str/blank?)
                                                             (mapv (fn [line]
                                                                     (mapv #(Long/parseLong %) (str/split line #" ")))))]
                                            [p1 program])))))
                           (some (fn [[p1 p2]]
                                   (when (and p1 p2)
                                     [p1 p2]))))]
    [part1 part2]))

(def all-ops
  {:addr (fn [mem a b c]
           (assoc mem c (+ (get mem a 0)
                           (get mem b 0))))
   :addi (fn [mem a b c]
           (assoc mem c (+ (get mem a 0)
                           b)))

   :mulr (fn [mem a b c]
           (assoc mem c (* (get mem a 0)
                           (get mem b 0))))
   :muli (fn [mem a b c]
           (assoc mem c (* (get mem a 0)
                           b)))

   :banr (fn [mem a b c]
           (assoc mem c (bit-and (get mem a 0)
                                 (get mem b 0))))
   :bani (fn [mem a b c]
           (assoc mem c (bit-and (get mem a 0)
                                 b)))

   :borr (fn [mem a b c]
           (assoc mem c (bit-or (get mem a 0)
                                (get mem b 0))))
   :bori (fn [mem a b c]
           (assoc mem c (bit-or (get mem a 0)
                                b)))
   :setr (fn [mem a _ c]
           (assoc mem c (get mem a 0)))

   :seti (fn [mem a _ c]
           (assoc mem c a))

   :gtir (fn [mem a b c]
           (assoc mem c (if (> a
                               (get mem b 0))
                          1
                          0)))

   :gtri (fn [mem a b c]
           (assoc mem c (if (> (get mem a 0)
                               b)
                          1
                          0)))

   :gtrr (fn [mem a b c]
           (assoc mem c (if (> (get mem a 0)
                               (get mem b 0))
                          1
                          0)))

   :eqir (fn [mem a b c]
           (assoc mem c (if (= a
                               (get mem b 0))
                          1
                          0)))
   :eqri (fn [mem a b c]
           (assoc mem c (if (= (get mem a 0)
                               b)
                          1
                          0)))
   :eqrr (fn [mem a b c]
           (assoc mem c (if (= (get mem a 0)
                               (get mem b 0))
                          1
                          0)))})

(defn possible [allowed-ops example]
  (->> (map (fn [op-kw]
              (find all-ops op-kw)) allowed-ops)
       (keep (fn [[op-kw op-fn]]
               (let [before (:before example)
                     inputs (rest (:op example))
                     after (:after example)]
                 (when (= (apply op-fn before inputs)
                          after)
                   op-kw))))))

(defn day16-1 [in]
  (let [[examples] (split-day16 in)
        result (->> examples
                    (filter (fn [example]
                              (let [pos-ops (possible (keys all-ops) example)]
                                (println "example" example)
                                (println "pos-ops" pos-ops)
                                (<= 3 (count pos-ops)))))
                    count)]
    result))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn day16-2 [in]
  (let [[part1 prog] (split-day16 in)
        code->ops (reduce
                   (fn [code->ops example]
                     (let [[op-code a b s] (:op example)]
                       (if-let [ops (get code->ops op-code)]
                         (let [pos-ops (possible ops example)]
                           (assoc code->ops op-code pos-ops))
                         (let [avail-ops (reduce disj (set (keys all-ops)) (vals code->ops))
                               pos-ops (possible avail-ops example)]
                           (assoc code->ops op-code pos-ops)))))
                   {}
                   part1)
        code->op (->> [{} code->ops]
                      (iterate (fn [[out code->ops]]
                                 (let [[code op] (some (fn [[code ops]]
                                                         (when (= (count ops) 1)
                                                           [code (first ops)]))
                                                       code->ops)]
                                   ;;(println "reduce" [code op])
                                   [(assoc out code op)
                                    (into {}
                                          (for [[c o] code->ops
                                                :when (not= code c)]
                                            [c (remove #{op} o)]))])))
                      #_(map (fn [[out co]]
                             (Thread/sleep 3000)
                             (println "==========")
                             (println "co" co)
                             (println "-------------")
                             (println "out" out)
                             [out co]))
                      (some (fn [[code->op _]]
                              (when (= (count code->op) 16)
                                code->op))))

        mem (reduce
             (fn [mem statement]
               (let [[code a b c] statement
                     op-kw (get code->op code)
                     op-fn (get all-ops op-kw)]
                 (op-fn mem a b c)))
             {}
             prog)
        zero-mem (get mem 0)]
    zero-mem))


(comment
  (day16-1 (io/resource "day16.txt"))

  (day16-2 (io/resource "day16.txt"))

  )


(defn print-day17 [world]
  (let [[xmin ymin] (:top-left world)
        [xmax ymax] (:bottom-right world)
        max-chars-per-cell (count (str xmax))
        headers (x/some
                 (comp (map (fn [n]
                              (let [s (str n)]
                                (str (apply str (repeat (- max-chars-per-cell (count s)) " ")) s))))
                       (x/transjuxt
                        (into []
                              (map (fn [i]
                                     (comp (map (fn [n-str]
                                                  (get n-str i)))
                                           (x/into []))))
                              (range max-chars-per-cell))))
                 (range xmin (inc xmax)))
        y-columns-offset (count (str ymax))
        ]
    (doseq [hr headers]
      (println (apply str (repeat y-columns-offset " ")) (apply str hr)))
    (doseq [y (range ymin (inc (inc ymax)))]
      (print y (apply str (repeat (- y-columns-offset (count (str y))) " ")))
      (doseq [x (range xmin (inc xmax))]
        (print (if (= (:spring world) [x y])
                 \+
                 (if (some (fn [[[sx sy] sstep]]
                             (= [sx sy] [x y])) (:sources world))
                   \S
                   (if (get (:clay world) [x y])
                     \#
                     (if (get (:water world) [x y])
                       \~
                       (if (get (:stream world) [x y])
                         \|
                         \.)))))))
      (println)))
  world)


(defn find-wall-corner [[sx sy] world dir]
  (let [xmin (first (:top-left world))
        xmax (first (:bottom-right world))
        [xmin xmax] (if (= dir :left)
                      [sx (dec xmin)]
                      [sx (inc xmax)])
        look-ahead (if (= dir :left)
                     dec
                     inc)]
    (reduce
     (fn [_ x]
       (if (or (contains? (:clay world) [x (inc sy)])
               (contains? (:water world) [x (inc sy)]))
         (if (get (:clay world) [(look-ahead x) sy])
           ;; found wall corner
           (reduced x)
           ;; go left or right
           nil)
         (reduced nil)))
     nil
     (range xmin xmax (if (= dir :left)
                        -1
                        1)))))

(defn fill [[sx sy] world]
  (when-let [x-left (find-wall-corner [sx sy] world :left)]
    (when-let [x-right (find-wall-corner [sx sy] world :right)]
      (let [y-wall (some
                    (fn [y]
                      (when (or (not (core/vec-contains? (get (:by-columns world) (dec x-left)) y))
                                (not (core/vec-contains? (get (:by-columns world) (inc x-right)) y)))
                        (inc y)))
                    (range sy 0 -1))
            y-ceiling (x/some
                       (comp (mapcat (fn [x-col]
                                       (get (:by-columns world) x-col)))
                             (filter (fn [y]
                                       (<= y sy)))
                             x/max)
                       (range x-left (inc x-right)))
            y-ceiling (if (and y-ceiling y-wall)
                        (max (inc y-ceiling) y-wall)
                        (if y-ceiling
                          (inc y-ceiling)
                          y-wall))]
        {:top-left [x-left y-ceiling]
         :bottom-right [x-right sy]}))))

(defn find-new-source [[sx sy] world dir]
  (let [xmin (first (:top-left world))
        xmax (first (:bottom-right world))
        ymax (second (:bottom-right world))
        [xmin xmax] (if (= dir :left)
                      [sx (dec xmin)]
                      [sx (inc xmax)])]
    (reduce
     (fn [_ x]
       (if-not (contains? (:clay world) [x sy])
         (if (and (not (contains? (:clay world) [x (inc sy)]))
                  (not (contains? (:water world) [x (inc sy)])))
           ;; found edge, drop down tetris style
           (let [new-y (x/some
                        (comp (filter (fn [y]
                                        (<= sy y)))
                              (x/wrap nil (+ ymax 2))
                              x/min
                              (map dec))
                        (get (:by-columns world) x))]
             (reduced [x new-y]))
           nil)
         (reduced nil)))
     nil
     (range xmin xmax (if (= dir :left)
                        -1
                        1)))))

(defn find-wall-x [[sx sy] world dir]
  (let [xmin (first (:top-left world))
        xmax (first (:bottom-right world))
        [xmin xmax] (if (= dir :left)
                      [sx (dec xmin)]
                      [sx (inc xmax)])
        look-ahead (if (= dir :left)
                     dec
                     inc)]
    (some
     (fn [x]
       (when (or (contains? (:clay world) [(look-ahead x) sy])
                 (= (look-ahead x) xmax))
         x))
     (range xmin xmax (if (= dir :left)
                        -1
                        1)))))

(defn move-source [[sx sy] world]
  (let [sstep (some (fn [[[x y] sstep]]
                      (when (= [sx sy] [x y])
                        sstep))
                    (:sources world))
        new-left-source (find-new-source [sx sy] world :left)
        new-right-source (find-new-source [sx sy] world :right)

        world (update world :sources (fn [sources]
                                       (cond-> sources
                                         (or new-left-source
                                             new-right-source)
                                         (dissoc [sx sy])
                                         new-left-source
                                         (assoc new-left-source sstep)
                                         new-right-source
                                         (assoc new-right-source sstep))))
        world (update world :stream (fn [stream]
                                      (cond-> (or stream #{})
                                        ;; sideways
                                        true
                                        (into (for [x (let [x-left (if new-left-source
                                                                     (first new-left-source)
                                                                     (find-wall-x [sx sy] world :left))
                                                            x-right (if new-right-source
                                                                      (first new-right-source)
                                                                      (find-wall-x [sx sy] world :right))]
                                                        (range x-left (inc x-right)))]
                                                [x sy]))
                                        ;; down left
                                        new-left-source
                                        (into (map (fn [y]
                                                     [(first new-left-source) y])
                                                   (range sy (inc (second new-left-source)))))
                                        ;; down right
                                        new-right-source
                                        (into (map (fn [y]
                                                     [(first new-right-source) y])
                                                   (range sy (inc (second new-right-source)))))
                                        )))]
    (when (= [sx sy] [327 1756])
      (println "move source?" [sx sy])
      (println "sources" (:sources world)))
    world))

(defn day17-1 [in]
  (let [world (into {:spring [500 0]}
                    (comp (mapcat (fn [line]
                                    (let [[_ from from-n-str to to-start-str to-end-str] (re-find #"([xy])=(\d+), ([xy])=(\d+)..(\d+)" line)
                                          [from-n to-start to-end] (map #(Long/parseLong %) [from-n-str to-start-str to-end-str])
                                          [from to] (map {"x" :x
                                                          "y" :y} [from to])]
                                      (for [j (range to-start (inc to-end))]
                                        (if (= from :x)
                                          [from-n j]
                                          [j from-n])))))
                          (x/transjuxt {:top-left (x/transjuxt [(comp (map first)
                                                                      x/min
                                                                      (map dec))
                                                                (comp (map second)
                                                                      ;; spring at 500,0
                                                                      (x/wrap 0 nil)
                                                                      x/min)])
                                        :bottom-right (x/transjuxt [(comp (map first)
                                                                          x/max
                                                                          (map inc))
                                                                    (comp (map second)
                                                                          x/max)])
                                        :clay (x/into #{})
                                        :by-columns (comp (x/by-key first ;; group-by x
                                                                    second ;; y
                                                                    (x/into []))
                                                          (x/into {}))}))
                    (xio/lines-in (core/string-in in)))

        world (assoc world
                     :sources {(:spring world) 0
                               ;;[502 9] 0
                               ;;[500 6] 0
                               })

        iter (fn [world]
               (let [{:keys [sources]} world
                     ;; find source with least filled reservoir
                     [[sx sy] sstep] (apply min-key (comp second first) ;; by y-val
                                            sources)

                     ;; top-left to bottom-right if block that can be filled with water from source
                     ;; if nil source needs to move
                     can-fill (fill [sx sy] world)]
                 (if-not can-fill
                   (move-source [sx sy] world)
                   (let [[tx ty] (:top-left can-fill)
                         [bx by] (:bottom-right can-fill)
                         source-new [sx (dec ty)]
                         water-count (* (inc (- bx tx))
                                        (inc (- by ty)))
                         world (update world :sources (fn [sources]
                                                        (-> sources
                                                            (dissoc [sx sy])
                                                            (assoc source-new (+ sstep water-count)))))
                         world (update world :water (fn [water]
                                                      (into (or water #{})
                                                            (for [x (range tx (inc bx))
                                                                  y (range ty (inc by))]
                                                              [x y]))))]
                     (move-source source-new world)))))

        top-y (apply min (map second (:clay world)))
        bottom-y (second (:bottom-right world))
        orig-world world
        result-world (->> world
                          (iterate iter)
                          #_(take 5)
                          #_(mapv print-day17)
                          (some (fn [world]
                                  (when (every?
                                         (fn [[[sx sy] step]]
                                           (<= bottom-y sy))
                                         (:sources world))
                                    (let [reachable (->> (into (set (:stream world))
                                                               (:water world))
                                                         (filter (fn [[x y]]
                                                                   (<= top-y y bottom-y))))
                                          part2 (count (:water world))]
                                      (assoc world
                                             :result (count reachable)
                                             :part2 part2))))))]
    (if (vector? result-world) ;; dev
      (last result-world)
      result-world)))


(comment
  (let [world (day17-1 (io/resource "day17.txt"))]
    (spit "day17world.txt"
          (with-out-str (print-day17 world))))

  (def w (day17-1
          "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
"))
  (print-day17 w)
  w

  (find-wall-corner [500 0] w :left)
  (find-wall-corner [500 0] w :right)
  (find-wall-corner [500 6] w :left)
  (find-wall-corner [500 6] w :right)

  (fill [500 0] w)
  (fill [500 6] w)

  (-> (day17-1
       "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504
")
      (print-day17))

  (let [world (day17-1 (io/resource "day17.txt"))]
    (println "result day17-1" (:result world))
    (println "result day17-2" (:part2 world))
    (spit "day17world.txt"
          (with-out-str (print-day17 world)))
    (def res world))


  (->> (slurp "day17world.txt")
       (filter (set "~"))
       count)

  (find-wall-x [500 4] w :left)
  (find-wall-corner [500 0] w :left)
  (find-wall-corner [500 0] w :right)
  (find-wall-corner [500 6] w :left)
  (find-wall-corner [500 6] w :right)

  (fill [500 0] w)
  (fill [500 6] w)

  (let [w (assoc w
                 :sources {[500 4] 10}
                 :water #{[500 5] [499 5] [498 5]})]
    (print-day17 w)
    (fill [500 4] w)
    ;;(find-wall-corner [500 4] w :left)
    ;;(find-wall-corner [500 4] w :right)
    )

  (let [w (assoc w :water #{[500 3] [499 3]
                            [500 5] [499 5] [498 5]})]
    (print-day17 w)
    [(find-new-source [500 2] w :left)
     (find-new-source [500 2] w :right)])


  )

(defn print-day18 [world]
  (println "minute: " (:minute world))
  (doseq [row (:field world)]
    (println (str/join "" row)))
  (println)
  world)

(defn day18-1 [in]
  (let [field (into []
                    (map vec)
                    (xio/lines-in (core/string-in in)))
        world {:minute 0
               :field field}

        iter (fn [world]
               (let [field (:field world)
                     field-new (vec (for [y (range (count field))]
                                      (vec (for [x (range (count (first field)))]
                                             (let [s (get-in field [y x])
                                                   neighbours (frequencies
                                                               (for [dx [-1 0 1]
                                                                     dy [-1 0 1]
                                                                     :when (not= 0 dx dy)
                                                                     :let [n (get-in field [(+ y dy) (+ x dx)])]
                                                                     :when n]
                                                                 n))]
                                               (cond
                                                 (= s \.)
                                                 (if (<= 3 (get neighbours \| 0))
                                                   \|
                                                   \.)

                                                 (= s \|)
                                                 (if (<= 3 (get neighbours \# 0))
                                                   \#
                                                   \|)

                                                 (= s \#)
                                                 (if (and (<= 1 (get neighbours \# 0))
                                                          (<= 1 (get neighbours \| 0)))
                                                   \#
                                                   \.)))))))]
                 {:minute (inc (:minute world))
                  :field field-new}))
        
        worlds (->> world
                    (iterate iter)
                    #_(take 300)
                    #_(mapv print-day18)
                    ;; part2
                    (reduce
                     (fn [seen {:keys [minute field]}]
                       (println "minute: " minute)
                       (if-let [prev-minute (get seen field)]
                         (reduced (let [loop-size (- minute prev-minute)
                                        _ (println "found loop size" loop-size)
                                        end-minute 1000000000
                                        togo (mod (- end-minute minute) loop-size)
                                        final-field-minute (+ prev-minute togo)
                                        final-field (some
                                                     (fn [[field minute]]
                                                       (when (= minute final-field-minute)
                                                         field))
                                                     seen)]
                                    (println "final-field-minute" final-field-minute "togo" togo "minute" minute "loop size" loop-size)
                                    #_(println "final-world" final-world)
                                    ;; return seq of worlds
                                    [{:minute 1000000000
                                      :field final-field}]))
                         (assoc seen field minute)))
                     {}))

        result {:worlds worlds
                :result (let [[trees lumber] (x/some
                                              (x/transjuxt [(comp cat
                                                                  (keep #{\|})
                                                                  x/count)
                                                            (comp cat
                                                                  (keep #{\#})
                                                                  x/count)])
                                              (:field (last worlds)))]
                          (println [trees lumber])
                          (* trees lumber))}]
    result))



(comment
  (-> (day18-1 ".#.#...|#.
.....#|##|
.|..|...#.
..|#.....#
#.#|||#|#|
...#.||...
.|....|...
||...#|.#|
|.||||..|.
...#.|..|.")
      :result)

  (-> (day18-1 (io/resource "day18.txt"))
      :result)

  (-> (day18-1 (io/resource "day18.txt"))
      :result)



  )

(defn print-day19 [ip mem]
  (println "ip=" (get mem ip) (str/join "," mem))
  (println "\n")
  mem)

(defn day19-1 [in]
  (let [prog (into []
                   (map (fn [line]
                          (if (= \# (first line))
                            [:pointer (Long/parseLong (second (re-find #"(\d+)" line)))]
                            (let [[op a b c] (str/split line #" ")
                                  [a b c] (map #(Long/parseLong %) [a b c])]
                              [(keyword op) a b c]))))
                   (xio/lines-in (core/string-in in)))
        [[_pointer ip]] prog
        prog (vec (rest prog))
        max-ip (count prog)

        STEP (atom 0)
        iter (fn [mem]
               (let [op-idx (get mem ip)
                     prog-line (get prog op-idx)
                     op-kw (first prog-line)
                     args (rest prog-line)
                     op (get all-ops op-kw)
                     mem-new (apply op mem args)
                     ip-new (get mem-new ip)
                     ip-new-inc (inc ip-new)
                     mem-ip (assoc mem-new ip ip-new-inc)]
                 (when (and (= op-idx 3)
                            (zero? (mod (swap! STEP inc) 100)))
                   (println mem))
                 
                 #_(println "res" op-idx prog-line op-kw)
                 (if (<= max-ip ip-new-inc)
                   (reduced (get mem-new 0))
                   mem-ip)))
        
        final (->> ;;(vec (repeat 6 0))
               [1 0 0 0 0 0]
               (iterate iter)
               ((fn [res]
                  (let [best (volatile! [0 0 0 0 0 0])]
                    (map (fn [mem]
                           (if (reduced? mem)
                             (do
                               (println "MAX mem:" @best)
                               mem)
                             (do
                               (vswap! best (fn [bmem]
                                              (mapv max bmem mem)))
                               mem)))
                         res))))
               ((fn [res]
                  (let [fq (volatile! {})]
                    (map (fn [mem]
                           (if (reduced? mem)
                             (do
                               (println "Command freq:" @fq)
                               mem)
                             (do
                               (vswap! fq (fn [fq]
                                            (let [pointer (get mem ip)]
                                              (update fq pointer (fnil inc 0)))))
                               mem)))
                         res))))
               #_(take 6)
               #_(mapv (partial print-day19 ip))
               (some (fn [mem]
                       (when (reduced? mem)
                         (unreduced mem)))))
        
        ]
    final))


(comment
  (day19-1 "#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5")

  [6 5 6 0 0 0]

  (day19-1 (io/resource "day19.txt"))

  [2072 877 877 767376 26 876]
  (->> {0 1, 7 12, 20 1, 1 1, 24 1, 4 767376, 15 875, 21 1, 13 876, 22 1, 6 767364, 25 1, 17 1, 3 767376, 12 876, 2 876, 23 1, 19 1, 11 766500, 9 767376, 5 767376, 14 876, 26 1, 16 1, 10 767376, 18 1, 8 767376}
       (sort-by first))
  ([0 1]
   [1 1]
   [2 876]
   [3 767376]
   [4 767376]
   [5 767376]
   [6 767364]
   [7 12]
   [8 767376]
   [9 767376]
   [10 767376]
   [11 766500]
   [12 876]
   [13 876]
   [14 876]
   [15 875]
   [16 1]
   [17 1]
   [18 1]
   [19 1]
   [20 1]
   [21 1]
   [22 1]
   [23 1]
   [24 1]
   [25 1]
   [26 1])

  ;;3-6
  seti 1 2 2   ;; reg 2 := 1
mulr 1 2 3     ;; reg 3 := reg 1 * reg 2
eqrr 3 5 3     ;; reg 3 := if (reg 5 = reg 3) 1 0
addr 3 4 4     ;; reg 4 := reg 3 + reg 4

;; 8-12
addr 1 0 0     ;; reg 0 := reg 0 + reg 1
addi 2 1 2     ;; reg 2 := reg 2 + 1
gtrr 2 5 3     ;; reg 3 := if (reg 2 > reg 5) 1 0
addr 4 3 4     ;; reg 4 := reg 3 + reg 4

  )

(defn print-day20 [world]
  (let [board (:board world)
        {:keys [top-left
                bottom-right]}
        (into {}
              (comp (map key)
                    (x/transjuxt {:top-left (x/transjuxt [(comp (map first)
                                                                x/min)
                                                          (comp (map second)
                                                                x/min)])
                                  :bottom-right (x/transjuxt [(comp (map first)
                                                                    x/max)
                                                              (comp (map second)
                                                                    x/max)])}))
              board)
        [xmin ymin] top-left
        [xmax ymax] bottom-right
        xrange (- xmax xmin)
        yrange (- ymax ymin)]
    ;;(println "top-left" top-left)
    ;;(println "bottom-right" bottom-right)
    ;;(println xrange yrange)
    ;;(println "\n")
    (doseq [j (range (inc (* 2 (inc yrange))))]
      (doseq [i (range (inc (* 2 (inc xrange))))]
        (print (if (and (odd? j)
                        (odd? i))
                 (let [x (+ (/ (dec i) 2) xmin)
                       y (+ (/ (dec j) 2) ymin)]
                   (if (= 0 y x)
                     \X
                     (if (or (contains? (get board [(dec x) y]) \E)
                             (contains? (get board [x (dec y)]) \S)
                             (contains? (get board [(inc x) y]) \W)
                             (contains? (get board [x (inc y)]) \N))
                         \.
                         \#)))
                 (if (and (even? j)
                          (odd? i))
                   ;; n/s
                   (let [[x y] [(+ xmin (/ (dec i) 2)) (+ ymin (/ j 2))]
                         above [x (dec y)]]
                     #_(println "n/s" [x y])
                     (if (and (contains? (get board [x y]) \N)
                              (contains? (get board above) \S))
                       \-
                       \#))
                   (if (and (odd? j)
                            (even? i))
                     ;; e/w
                     (let [[x y] [(dec (+ xmin (/ i 2))) (+ ymin (/ (dec j) 2))]
                           right [(inc x) y]]
                       (if (or (contains? (get board [x y]) \E)
                               (contains? (get board right) \W))
                         \|
                         \#))
                     \#)))))
      (println))
    )
  world)

(defn day20-1 [regex]
  (let [z (zip/zipper :children
                      (comp seq :children)
                      (fn [node children]
                        (assoc node :children children))
                      {:dir nil
                       :children []})

        to-begin-of-branch (fn [z]
                             (->> z
                                  (iterate zip/up)
                                  (some (fn [z]
                                          (when (not (:tail (zip/node z)))
                                            z)))))
        z (reduce
           (fn [z ch]
             (cond
               (= ch \^)
               z

               (contains? (set "NWES") ch)
               (if (seq (:children (zip/node z)))
                 (-> z
                     (zip/insert-child {:dir (str ch)
                                        :tail true
                                        :children []})
                     zip/down)
                 (zip/edit z update :dir str ch))

               ;; always a char before and after
               (= ch \()
               (-> z
                   (zip/insert-child {:dir nil
                                      :children []})
                   zip/down)

               ;; |))) - )))|
               (= ch \))
               (if (:dir (zip/node z))
                 (-> z
                     to-begin-of-branch
                     zip/up)
                 ;; ) after |
                 (zip/remove z))

               (= ch \|)
               (-> z
                   to-begin-of-branch
                   (zip/insert-left {:dir nil
                                     :children []})
                   (zip/left))

               (= ch \$)
               (zip/root z)

               (= ch \newline)
               z))
           z
           regex)

        dir-step {\E [1 0]
                  \N [0 -1]
                  \W [-1 0]
                  \S [0 1]}
        
        board (->> [[[z [0 0]]] {}]
                   (iterate (fn [[todo board]]
                              (let [[tree at] (first todo)
                                    ch (first (:dir tree))

                                    atx (get dir-step ch)
                                    next-at (mapv + at atx)
                                    board (update board at (fnil conj #{}) ch)
                                    tree-leftover (when (next (:dir tree))
                                                    (update tree :dir subs 1))
                                    todo (if tree-leftover
                                           (conj (rest todo)
                                                 [tree-leftover next-at])
                                           (into (rest todo)
                                                 (map (fn [child]
                                                        [child next-at])
                                                      (:children tree))))]

                                [todo board])))
                   (some (fn [[z board]]
                           (when (not (seq z))
                             board))))

        dir-board (into {}
                        (for [[[x y] dirs] board]
                          [[x y] (into #{}
                                       (map (fn [ch]
                                              (mapv +
                                                    (get dir-step ch)
                                                    [x y])))
                                       dirs)]))

        [furthest-door door-two] (reduce
                                  (fn [{:keys [seen-xy-n active]} door-n]
                                    (if (seq active)
                                      (let [seen-xy-n (into seen-xy-n
                                                            (map (fn [xy]
                                                                   [xy door-n]))
                                                            active)
                                            active (into #{}
                                                         (comp (mapcat (fn [[x y]]
                                                                         (let [neighbours (get dir-board [x y])]
                                                                           neighbours)))
                                                               (remove seen-xy-n))
                                                         active)]
                                        {:seen-xy-n seen-xy-n
                                         :active active})
                                      (let [furthest-door (dec door-n)
                                            far (count (filter (fn [[xy n]]
                                                                 (<= 1000 n))
                                                               seen-xy-n))]
                                        (reduced [furthest-door
                                                  far]))))
                                  {:seen-xy-n {}
                                   :active [[0 0]]}
                                  (range))]
    {:z z
     :board board
     :dir-board dir-board
     :door furthest-door
     :door-two door-two}))


(comment
  (-> (day20-1 "^ENWWWS$")
      print-day20)

  (-> (day20-1 "^WNE$")
      print-day20)
  
  (-> (day20-1 "^ENWWW(NEEE|S)$")
      print-day20)

  (-> (day20-1 "^ENWWW(NEEE|SSE(EE|N))$")
      print-day20)
  ;; 10 doors
  ;; #########
  ;; #.|.|.|.#
  ;; #-#######
  ;; #.|.|.|.#
  ;; #-#####-#
  ;; #.#.#X|.#
  ;; #-#-#####
  ;; #.|.|.|.#
  ;; #########

  (-> (day20-1 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
      print-day20)
  ;; 18 doors
  ;; ###########
  ;; #.|.#.|.#.#
  ;; #-###-#-#-#
  ;; #.|.|.#.#.#
  ;; #-#####-#-#
  ;; #.#.#X|.#.#
  ;; #-#-#####-#
  ;; #.#.|.|.|.#
  ;; #-###-###-#
  ;; #.|.|.#.|.#
  ;; ###########


  (-> (day20-1
       ;;"^ESSWWN(E|NNENN)$"
       "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
       )
      print-day20)
  ;; Furthest room requires passing 23 doors

  ;; #############
  ;; #.|.|.|.|.|.#
  ;; #-#####-###-#
  ;; #.#.|.#.#.#.#
  ;; #-#-###-#-#-#
  ;; #.#.#.|.#.|.#
  ;; #-#-#-#####-#
  ;; #.#.#.#X|.#.#
  ;; #-#-#-###-#-#
  ;; #.|.#.|.#.#.#
  ;; ###-#-###-#-#
  ;; #.|.#.|.|.#.#
  ;; #############


  (-> (day20-1 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
      print-day20)
  ;; Furthest room requires passing 31 doors

  ;; ###############
  ;; #.|.|.|.#.|.|.#
  ;; #-###-###-#-#-#
  ;; #.|.#.|.|.#.#.#
  ;; #-#########-#-#
  ;; #.#.|.|.|.|.#.#
  ;; #-#-#########-#
  ;; #.#.#.|X#.|.#.#
  ;; ###-#-###-#-#-#
  ;; #.|.#.#.|.#.|.#
  ;; #-###-#####-###
  ;; #.|.#.|.|.#.#.#
  ;; #-#-#####-#-#-#
  ;; #.#.|.|.|.#.|.#
  ;; ###############
  
  (-> (day20-1 "^ENWWW(NEEE)$")
      print-day20)

  (-> (day20-1 "^EN$")
      print-day20)
  
  (-> (day20-1 "^ENWWW$")
      print-day20)

  (-> (day20-1 "^E(S|W)$")
      print-day20)

  (-> (day20-1
       "^N(WW|EE(SWEN|)NNNN(ESSNNW|)WW(SE|NENW))$")
      print-day20)


  (-> (day20-1 "^EE(NNEESSSSW(SS|WW|NN|)WWWNNNNEE(N|))$")
      print-day20)

  (-> (day20-1 "^EE(NNEESSSSW(SS|WW|NN|)WWWNNNNEE(N|))$")
      print-day20)


  (let [res (day20-1
             (slurp (io/resource "day20.txt")))]
    (println "door" (:door res) " door-two" (:door-two res))
    (spit "day20.txt"
          (with-out-str (print-day20 res))))
  )
