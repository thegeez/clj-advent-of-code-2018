(ns advent2018.core3
  (:require [advent2018.core :as core]
            [advent2018.core2 :as core2]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]
            [net.cgrand.xforms.rfs :as xrf]))

(set! *warn-on-reflection* true)

;; builds on day 16 & 19

(defn day21-1 [in]
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
        seen (volatile! #{})
        prev (volatile! nil)
        iter (fn [mem]
               (let [op-idx (get mem ip)
                     prog-line (get prog op-idx)
                     op-kw (first prog-line)
                     args (rest prog-line)
                     op (get core2/all-ops op-kw)
                     mem-new (apply op mem args)
                     ip-new (get mem-new ip)
                     ip-new-inc (inc ip-new)
                     mem-ip (assoc mem-new ip ip-new-inc)]
                 (if (= op-idx 28)
                   ;; part1
                   #_(reduced mem)
                   ;;part2
                   (let [reg5 (get mem 5)
                         ;;_ (println @STEP "seen" (count @seen) "r5" reg5 "?" (contains? @seen reg5))
                         ]
                     (if (contains? @seen reg5)
                       (reduced @prev)
                       (do
                         (vswap! seen conj reg5)
                         (vreset! prev reg5)
                         mem-ip)))
                   mem-ip)))
        
        final (->> [0 0 0 0 0 0]
                   (iterate iter)
                   (some (fn [mem]
                           (when (reduced? mem)
                             (unreduced mem)))))
        ]
    final))

(comment

  (day21-1 (io/resource "day21.txt"))

  )

(defn print-day22 [{:keys [cave] :as res}]
  (println "\n")
  (doseq [y (range (count cave))]
    (doseq [x (range (count (first cave)))]
      (let [type (get-in cave [y x
                               :type
                               ])
            type-ch (get {0 \.
                          1 \=
                          2 \|} type)]
        (print type-ch)))
    (println))
  res)

(defn day22-1 [depth [target-x target-y]]
  (let [cave (reduce
              (fn [cave [x y]]
                (let [index (if (or (= [x y] [0 0])
                                    (= [x y] [target-x target-y]))
                              0
                              (if (= x 0)
                                (* y 48271N)
                                (if (= y 0)
                                  (* x 16807N)
                                  (let [left (get-in cave [y (dec x) :level])
                                        top (get-in cave [(dec y) x :level])]
                                    (* left top)))))
                      level (mod (+ depth index) 20183)
                      type (mod level 3)]
                  (assoc-in cave [y x] {:type type
                                        :level level})))
              (vec (repeat (inc target-y)
                           (vec (repeat (inc target-x) nil))))
              (for [y (range (inc target-y))
                    x (range (inc target-x))]
                [x y]))

        sum (->> (flatten cave)
                 (map :type)
                 (reduce +))]
    {:sum sum
     :cave cave}))

(defn extend-cave [cave depth row-or-col]
  (let [res (let [maxy (count cave)
                  maxx (count (first cave))]
              (reduce
               (fn [cave [x y]]
                 (let [index (if (= x 0)
                               (* y 48271N)
                               (if (= y 0)
                                 (* x 16807N)
                                 (let [left (get-in cave [y (dec x) :level])
                                       top (get-in cave [(dec y) x :level])]
                                   (* left top))))
                       level (mod (+ depth index) 20183)
                       type (mod level 3)
                       cave (assoc-in cave [y x] {:type type
                                                  :level level})]
                   cave))
               (if (= row-or-col :row)
                 (conj cave (vec (repeat maxx nil)))
                 cave)
               (if (= row-or-col :col)
                 (for [y (range maxy)]
                   [maxx y])
                 (for [x (range maxx)]
                   [x maxy]))))]
    (if (= row-or-col :col)
      (assert (and (= (count cave) (count res))
                   (= (->> (map count cave)
                           (mapv inc))
                      (mapv count res)))
              [(count cave) (mapv count cave)
               (count res) (mapv count res)]))
    res))

(defn day22-2 [depth [^long target-x ^long target-y]]
  (let [cave (:cave (day22-1 depth [target-x target-y]))

        allowed-tool
        (fn [tool type]
          (cond
            ;; rocky
            (= type 0)
            (or (= tool :torch)
                (= tool :climbing))

            ;; wet
            (= type 1)
            (or (= tool :neither)
                (= tool :climbing))

            ;; narrow
            (= type 2)
            (or (= tool :neither)
                (= tool :torch))))

        start [[0 0] :torch]
        target [[target-x target-y] :torch]
        STEP (atom 0)
        res (loop [cave cave
                   seen #{}
                   fringe
                   {start 0}
                   ]
              (let [[[[x y] tool] time]
                    (apply min-key (fn [[[[^long x ^long y] tool] time]]
                                     (+ time
                                        (+ (Math/abs (- target-x x))
                                           (Math/abs (- target-y y)))))
                           fringe)

                    cave (if (= x (dec (count (first cave))))
                           (extend-cave cave depth :col)
                           cave)
                    cave (if (= y (dec (count cave)))
                           (extend-cave cave depth :row)
                           cave)

                    next-at (for [[dx dy] [[-1 0] [1 0] [0 1] [0 -1]]
                                  :let [[x y] [(+ x dx) (+ y dy)]]
                                  :when (and (<= 0 x)
                                             (<= 0 y))
                                  :let [to-type (get-in cave [y x :type])
                                        _ (assert to-type
                                                  (str "cave needs extending" [x y]
                                                       (count cave)
                                                       (mapv count cave)))]
                                  :when (allowed-tool tool to-type)
                                  :when (not (contains? seen [[x y] tool]))
                                  ]
                              [[[x y] tool] (+ time 1)])
                    at-type (get-in cave [y x :type])
                    next-tool (for [to-tool [:torch :climbing :neither]
                                    :when (allowed-tool to-tool at-type)
                                    :when (not (contains? seen [[x y] to-tool]))]
                                [[[x y] to-tool] (+ time 7)])
                    next-at (into next-at
                                  next-tool)]
                (if (and (= [x y] [target-x target-y])
                         (= tool :torch))
                  time
                  (recur cave
                         (conj seen [[x y] tool])
                         (merge-with min
                                     (dissoc fringe [[x y] tool])
                                     (into {} next-at))))))]
    res))

(comment
  (-> (day22-1 510
               #_[2 2]
               [10 10])
      (print-day22 ))

  (-> (day22-1 6084
               [14 709])
      :sum)

  ;; travel 45
  
  ;; M=.|=.|.|=.|=|=.
  ;; .|=|=|||..|.=...
  ;; .==|....||=..|==
  ;; =.|....|.==.|==.
  ;; =|..==...=.|==..
  ;; =||.=.=||=|=..|=
  ;; |.=.===|||..=..|
  ;; |..==||=.|==|===
  ;; .=..===..=|.|||.
  ;; .======|||=|=.|=
  ;; .===|=|===T===||
  ;; =|||...|==..|=.|
  ;; =.=|=.=..=.||==|
  ;; ||=|=...|==.=|==
  ;; |=.=||===.|||===
  ;; ||.|==.|.|.||=||

  (day22-2 510 [10 10])

  (day22-2 6084
           [14 709])
  )
