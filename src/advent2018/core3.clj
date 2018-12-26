(ns advent2018.core3
  (:require [advent2018.core :as core]
            [advent2018.core2 :as core2]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]
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


(defn print-day23 [world]
  )


(defn day23-1 [in]
  (let [bots (into {}
                   (map-indexed (fn [id line]
                                  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
                                        [x y z r] (map #(Long/parseLong %) [x y z r])]
                                    [id [x y z r]])))
                   (xio/lines-in (core/string-in in)))

        powerest (apply max-key (fn [[id [x y z r]]] r) bots)

        reachable (fn [[fid [^long fx ^long fy ^long fz ^long fr]]
                       [tid [^long tx ^long ty ^long tz _]]]
                    (let [dist (+ (Math/abs (- fx tx))
                                  (Math/abs (- fy ty))
                                  (Math/abs (- fz tz)))]
                      (println "dist" [])
                      (<= dist fr)))
        
        in-reach (for [bot bots
                       :when (reachable powerest bot)]
                   bot)]
    (count in-reach)))

#_(defn day23-2 [in]
  (let [bots (into {}
                   (map-indexed (fn [id line]
                                  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
                                        [x y z r] (map #(Long/parseLong %) [x y z r])]
                                    [id [x y z r]])))
                   (xio/lines-in (core/string-in in)))

        powerest (apply max-key (fn [[id [x y z r]]] r) bots)

        overlap (fn [[fid [^long fx ^long fy ^long fz ^long fr]]
                     [tid [^long tx ^long ty ^long tz ^long tr]]]
                  (let [dist (+ (Math/abs (- fx tx))
                                (Math/abs (- fy ty))
                                (Math/abs (- fz tz)))
                        max-dist (+ fr tr)]
                    (<= dist max-dist)))
        pairs (into {}
                    (x/by-key first
                              second
                              (x/into []))
                    (for [[from & others] (->> (iterate rest bots)
                                               (take-while seq))
                          other others
                          :when (overlap from other)]
                      [from other]))
        combos (loop [combos []
                      expand (mapv (fn [bot]
                                     [bot])
                                   bots)]
                 (if-not (seq expand)
                   combos
                   (let [e (first expand)
                         from (peek e)
                         add (get pairs from)]
                     
                     (println "e" e "add" add)
                     (if (seq add)
                       (recur combos
                              (into (rest expand)
                                    (keep
                                     (fn [add-one]
                                       (when (every? (fn [from]
                                                       (overlap from add-one)) e)
                                         (println "extend yes")
                                         (conj e add-one)))
                                     add)))
                       (recur (conj combos e)
                              (rest expand))))))
        most-combo (apply max-key count combos)]
    (def combos combos)
    (def pairs pairs)
    most-combo
    ;;combos
    ))

;; h/t https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecdqzdg/

(defn day23-2 [in]
  (let [bots (into {}
                   (map-indexed (fn [id line]
                                  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
                                        [x y z r] (map #(Long/parseLong %) [x y z r])]
                                    [id [x y z r]])))
                   (xio/lines-in (core/string-in in)))

        q (into (sorted-map) ;; store dups by adding id to key
                (mapcat (fn [[id [x y z r]]]
                          (let [d (+ (Math/abs ^long x)
                                     (Math/abs ^long y)
                                     (Math/abs ^long z))]
                            [[[(max 0 (- d r)) id] 1]
                             [[(+ d r) id] -1]])))
                bots)

        res (->> (reductions
                  (fn [[c _dist] [[dist id] e]]
                    [(+ c e) dist])
                  [0 nil]
                  q)
                 rest
                 (apply max-key first)
                 second)]
    res))

(comment
  
(day23-1 "pos=<0,0,0>, r=4
pos=<1,0,0>, r=1
pos=<4,0,0>, r=3
pos=<0,2,0>, r=1
pos=<0,5,0>, r=3
pos=<0,0,3>, r=1
pos=<1,1,1>, r=1
pos=<1,1,2>, r=1
pos=<1,3,1>, r=1")


(day23-2 "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5")

combos
pairs

;; 12,12,12 -> 36

(day23-1 (io/resource "day23.txt"))

(day23-2 (io/resource "day23.txt"))

  )

(defn day24-1 [in]
  (let [lines (into []
                    (xio/lines-in (core/string-in in)))
        line-group (->> (reduce
                         (fn [{:keys [group lines id]} ^String line]
                           (cond
                             (.startsWith line "Immune System")
                             {:group :immune
                              :id 1
                              :lines lines}
                             (.startsWith line "Infection")
                             {:group :infection
                              :id 1
                              :lines lines}
                             (str/blank? line)
                             {:group group
                              :lines lines}
                             true
                             {:group group
                              :id (inc id)
                              :lines (conj lines {:group group
                                                  :id id
                                                  :line line})}
                             ))
                         {:group nil
                          :lines []}
                         lines)
                        :lines)
        groups (mapv
                (fn [lg]
                  (let [[units _units _each _with hitpoints _hit _points & tail] (str/split (:line lg) #" ")
                        [units hitpoints] (map #(Long/parseLong %) [units hitpoints])
                        {:keys [immune weak]} (when-let [[_ spec] (re-find #"\(([^\)]*)\)" (:line lg))]
                                                (into {}
                                                      (for [piece (str/split spec #"; ")]
                                                        (let [[type _to & etc] (str/split piece #" ")
                                                              props (str/split (apply str etc) #",")]
                                                          [(keyword type) (set (mapv keyword props))]))))
                        attack (subs (:line lg) (.indexOf ^String (:line lg) "attack"))
                        [_attack _that _does damage-n damage-type _damage _at _initiative initiative] (str/split attack #" ")
                        damage (Long/parseLong damage-n)
                        damage-type (keyword damage-type)
                        initiative (Long/parseLong initiative)]
                    (-> lg
                        (dissoc :line)
                        (assoc :units units
                               :hitpoints hitpoints
                               :weak weak
                               :immune immune
                               :damage damage
                               :damage-type damage-type
                               :initiative initiative
                               ))))
                line-group)

        select-power-key (fn [{:keys [units damage initiative]}]
                           [(* units damage) initiative])

        effective-damage (fn [attacker-group opponent-group]
                           (let [attacker-type (:damage-type attacker-group)
                                 eff-power (* (:units attacker-group)
                                              (:damage attacker-group))]
                             (cond
                               (contains? (:weak opponent-group) attacker-type)
                               (* eff-power 2)
                               (contains? (:immune opponent-group) attacker-type)
                               0
                               true
                               eff-power)))

        fight (fn [groups]
                (let [order (into (sorted-set-by (fn [l r]
                                                   (* -1 ;; highest to lowest
                                                      (compare  (select-power-key l)
                                                                (select-power-key r)))))
                                  groups)

                      target-selection (-> (reduce
                                            (fn [[attack defend] attacker]
                                              (let [{attacker-group :group
                                                     attacker-type :damage-type} attacker
                                                    opponent (x/some
                                                              (comp (x/maximum (fn [l r]
                                                                                 (compare (first l)
                                                                                          (first r))))
                                                                    (map second))
                                                              (for [g groups
                                                                    :when (not= attacker-group
                                                                                (:group g))
                                                                    :when (not (contains? defend [(:group g) (:id g)]))
                                                                    :let [eff-damage (effective-damage attacker g)]
                                                                    :when (pos? eff-damage)]
                                                                (let [defence-eff-damage (* (:units g)
                                                                                            (:damage g))]
                                                                  [[eff-damage
                                                                    defence-eff-damage
                                                                    (:initiative g)] g])))]
                                                (if opponent
                                                  [(assoc attack [attacker-group (:id attacker)] [(:group opponent) (:id opponent)])
                                                   (conj defend [(:group opponent) (:id opponent)])]
                                                  [attack defend])))
                                            [{} #{}]
                                            order)
                                           first)

                      groups-by-id (zipmap (map (juxt :group :id) groups)
                                           groups)

                      attack-order (-> (into (sorted-map)
                                             (x/by-key (comp #(* -1 %) :initiative)
                                                       (map (juxt :group :id)))
                                             groups)
                                       vals)

                      deal-attack (reduce
                                   (fn [groups-by-id gid]
                                     (let [attack-group (get groups-by-id gid)
                                           opponent-gid (get target-selection gid)
                                           opponent (get groups-by-id opponent-gid)]
                                       (if (not (and attack-group opponent))
                                         groups-by-id
                                         (let [total-damage (effective-damage attack-group opponent)
                                               kill (min (long (Math/floor (/ total-damage
                                                                              (:hitpoints opponent))))
                                                         (:units opponent))
                                               rem-units (- (:units opponent)
                                                            kill)
                                               dead (= 0 rem-units)]
                                           (if dead
                                             (dissoc groups-by-id opponent-gid)
                                             (assoc-in groups-by-id [opponent-gid :units] rem-units))))))
                                   groups-by-id
                                   attack-order)

                      groups (vals deal-attack)]
                  groups))

        #_part1 #_(->> groups
                       (iterate fight)
                       (some (fn [groups]
                               (let [[ac bc] (->> (into {}
                                                        (x/by-key :group
                                                                  :units
                                                                  (x/reduce +))
                                                        groups)
                                                  vals)]
                                 (println "found end: " ac bc)
                                 (if (not ac)
                                   bc
                                   (if (not bc)
                                     ac
                                     nil))))))

        part2 (reduce
               (fn [_ boost]
                 (let [groups (mapv (fn [g]
                                      (if (= (:group g) :immune)
                                        (update g :damage + boost)
                                        g))
                                    groups)]
                   (->> groups
                        (iterate fight)
                        (partition 2 1)
                        (some (fn [[l r]]
                                (if (= l r)
                                  ;; invinsible immune system, continue
                                  :continue
                                  (let [groups r
                                        {:keys [immune infection]} (into {}
                                                                         (x/by-key :group
                                                                                   :units
                                                                                   (x/reduce +))
                                                                         groups)]
                                    (if (and immune
                                             (not infection))
                                      (reduced {:boost boost
                                                :immune immune})
                                      (if (not immune)
                                        :try-next
                                        nil)))))))))
               nil
               (range))
        ]
    part2))

(comment
  (day24-1 "Immune System:
17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3

Infection:
801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")

  (day24-1 (io/resource "day24.txt"))

  )


(defn day25 [in]
  (let [stars (into []
                    (map (fn [line]
                           (let [[a b c d] (-> line
                                               (str/trim)
                                               (str/split #",")
                                               (->> (map #(Long/parseLong %))))]
                             [a b c d])))
                    (xio/lines-in (core/string-in in)))

        reachable (fn [[^long la ^long lb ^long lc ^long ld]
                       [^long ra ^long rb ^long rc ^long rd]]
                    (<= (+ (Math/abs (- la ra))
                           (Math/abs (- lb rb))
                           (Math/abs (- lc rc))
                           (Math/abs (- ld rd)))
                        3))

        consts (reduce
                (fn [consts star]
                  (let [join (for [c consts
                                   s c
                                   :when (reachable star s)]
                               c)
                        rem (reduce
                             disj
                             consts
                             join)
                        add (reduce
                             into
                             #{star}
                             join)]
                    (conj rem add)))

                #{}
                stars)]
    (count consts)))


(test/deftest day25-test
  (test/are [in out] (= (day25 in) out)
    "0,0,0,0
 3,0,0,0
 0,3,0,0
 0,0,3,0
 0,0,0,3
 0,0,0,6
 9,0,0,0
12,0,0,0" 2

    "-1,2,2,0
0,0,2,-2
0,0,0,-2
-1,2,0,0
-2,-2,-2,2
3,0,2,-1
-1,3,2,2
-1,0,-1,0
0,2,1,-2
3,0,0,0" 4

    "1,-1,0,1
2,0,-1,0
3,2,-1,0
0,0,3,1
0,0,-1,-1
2,3,-2,0
-2,2,0,0
2,-2,0,-1
1,-1,0,-1
3,2,0,2" 3

    "1,-1,-1,-2
-2,-2,0,1
0,2,1,3
-2,3,-2,1
0,2,3,-2
-1,-1,1,-2
0,-2,-1,0
-2,2,3,-1
1,2,2,0
-1,-2,0,-2" 8
    ))


(comment
  (day25-test)

  (day25 "0,0,0,0
 3,0,0,0
 0,3,0,0
 0,0,3,0
 0,0,0,3
 0,0,0,6
 9,0,0,0
12,0,0,0")


  (day25 (io/resource "day25.txt"))
  )


(def dirs
  {\╠ [[0 -1] [1 0] [0 1]]
   \╣ [[0 -1] [0 1] [-1 0]]
   \╦ [[1 0] [0 1] [-1 0]]
   \╩ [[0 -1] [1 0] [-1 0]]
   \╬ [[0 -1] [1 0] [0 1] [-1 0]]
   \═ [[1 0] [-1 0]]
   \║ [[0 -1] [0 1]]
   \╔ [[1 0] [0 1]]
   \╗ [[-1 0] [0 1]]
   \╚ [[0 -1] [1 0]]
   \╝ [[0 -1] [-1 0]]})

(defn infi [in]
  (let [tiles (into {}
                    (comp (map-indexed (fn [y row]
                                         (map-indexed (fn [x col]
                                                        [[x y] col])
                                                      row)))
                          cat)
                    (xio/lines-in (core/string-in in)))
        goal (x/some
              (x/transjuxt [(comp (map first) x/max)
                            (comp (map second) x/max)])
              (keys tiles))

        start [0 0]

        steps (loop [seen {}
                     expand [[start 0]]]
                (if-let [done (get seen goal)]
                  done
                  (let [seen (into seen expand)
                        expand (mapcat
                                (fn [[[x y :as xy] dist]]
                                  (let [tile (get tiles xy)]
                                    (for [[dx dy] (get dirs tile)
                                          :let [x (+ x dx)
                                                y (+ y dy)]
                                          :when (and (<= 0 x (first goal))
                                                     (<= 0 y (second goal)))
                                          :let [opposite (get tiles [x y])
                                                opp-dirs (get dirs opposite)]
                                          :when (some (fn [od]
                                                        (= [dx dy] (mapv #(* -1 %) od)))
                                                      opp-dirs)
                                          :when (not (contains? seen [x y]))]
                                      [[x y] (inc dist)])))
                                expand)]
                    (recur seen
                           expand))))]
    steps))

(defn infi2 [in]
  (let [tiles (into {}
                    (comp (map-indexed (fn [y row]
                                         (map-indexed (fn [x col]
                                                        [[x y] col])
                                                      row)))
                          cat)
                    (xio/lines-in (core/string-in in)))
        goal (x/some
              (x/transjuxt [(comp (map first) x/max)
                            (comp (map second) x/max)])
              (keys tiles))

        wrap (inc (first goal))

        start [0 0]


        shift (fn [[x y] tiles change]
                (let [row-or-col (mod change wrap)
                      action (if (even? change)
                               ;; shift row
                               (fn [[[fx fy] tile]]
                                 (if (= fy row-or-col)
                                   [[(mod (inc fx) wrap) fy] tile]
                                   [[fx fy] tile]))
                               ;; shift col
                               (fn [[[fx fy] tile]]
                                 (if (= fx row-or-col)
                                   [[fx (mod (inc fy) wrap)] tile]
                                   [[fx fy] tile])))
                      new-tiles (into {}
                                      (map action)
                                      tiles)
                      new-xy (if (= [x y] goal)
                               [x y]
                               (if (even? change)
                                 (if (= y row-or-col)
                                   [(mod (inc x) wrap) y]
                                   [x y])
                                 (if (= x row-or-col)
                                   [x (mod (inc y) wrap)]
                                   [x y])))]
                  [new-xy new-tiles]))

        step (fn [fringe]
               (let [[dist [x y :as xy] tiles change] (first fringe)
                     expanded (let [tile (get tiles xy)]
                                (for [[dx dy] (get dirs tile)
                                      :let [x (+ x dx)
                                            y (+ y dy)]
                                      :when (and (<= 0 x (first goal))
                                                 (<= 0 y (second goal)))
                                      :let [opposite (get tiles [x y])
                                            opp-dirs (get dirs opposite)]
                                      :when (some (fn [od]
                                                    (= [dx dy] (mapv #(* -1 %) od)))
                                                  opp-dirs)]
                                  (let [[xy tiles] (shift [x y] tiles dist)
                                        dist (inc dist)]
                                    [dist xy tiles change])))]
                 (into (disj fringe (first fringe))
                       expanded)))

        steps (->> (conj (sorted-set-by (fn [l r]
                                          (compare [(first l) (second l)]
                                                   [(first r) (second r)])))
                         [0 start tiles 0])
                   (iterate step)
                   (some (fn [fringe]
                           (let [first-entry (first fringe)
                                 [best xy tiles change] first-entry]
                             (when (= xy goal)
                               best)))))]
    steps))

(comment
  (infi2 (io/resource "infi.txt"))


  (infi2
   "╔═╗║
╠╗╠║
╬╬╣╬
╚╩╩═")


  )
