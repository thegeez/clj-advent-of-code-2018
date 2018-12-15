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

