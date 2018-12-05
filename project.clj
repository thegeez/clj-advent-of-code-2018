(defproject advent2018 "0.0.1"
  :description "Advent of Code w/ Clojure"
  :url "https://thegeez.net"
  :dependencies [[org.clojure/clojure "1.10.0-RC2"]
                 [net.cgrand/xforms "0.18.2"]
                 ;;[com.clojure-goes-fast/clj-async-profiler "0.2.0"]
                 ]

  ;; for clj-async-profiler
;;  :jvm-opts ["-Djdk.attach.allowAttachSelf"]
  
  :plugins [[cider/cider-nrepl "0.17.0"]])
