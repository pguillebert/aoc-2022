(ns aoc2022.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def file "input4.txt")

(defn expand-ranges
  [[a b]]
  (let [[a1 a2] (str/split a #"-")
        expa (range (Integer/parseInt a1) (inc (Integer/parseInt a2)))
        [b1 b2] (str/split b #"-")
        expb (range (Integer/parseInt b1) (inc (Integer/parseInt b2)))]
    [(into #{} expa) (into #{} expb)]))

(defn find-contained
  [[r1set r2set]]
  (or
   (set/subset? r1set r2set)
   (set/subset? r2set r1set)))

(defn find-overlap
  [[r1set r2set]]
  (not (empty? (set/intersection r1set r2set))))

;; either call with find-contained or find-overlap
(defn day4
  [func]
  (let [lines (-> (io/resource file)
                  (slurp)
                  (str/split #"\n"))
        pairs (map #(str/split % #",") lines)]
    (->> pairs
;;         (take 4)
         (map expand-ranges)
         (map func)
         (filter identity)
         (count))))
