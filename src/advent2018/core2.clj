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
