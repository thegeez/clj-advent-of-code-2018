(ns advent2018.core3
  (:require [advent2018.core :as core]
            [advent2018.core2 :as core2]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [net.cgrand.xforms :as x]
            [net.cgrand.xforms.io :as xio]
            [net.cgrand.xforms.rfs :as xrf]))


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
