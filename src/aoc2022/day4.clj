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
    [expa expb]))

(defn find-contained
  [[r1 r2]]
  (let [r1set (into #{} r1)
        r2set (into #{} r2)]
    (or
     (set/subset? r1set r2set)
     (set/subset? r2set r1set))))

(defn find-overlap
  [[r1 r2]]
  (let [r1set (into #{} r1)
        r2set (into #{} r2)
        inter (set/intersection r1set r2set)]
    (not (empty? inter))))

(defn day4-1
  []
  (let [lines (-> (io/resource file)
                  (slurp)
                  (str/split #"\n"))
        pairs (map #(str/split % #",") lines)]
    (->> pairs
;;         (take 4)
         (map expand-ranges)
         (map find-contained)
         (filter identity)
         (count))))

(defn day4-2
  []
  (let [lines (-> (io/resource file)
                  (slurp)
                  (str/split #"\n"))
        pairs (map #(str/split % #",") lines)]
    (->> pairs
;;         (take 20)
         (map expand-ranges)
         (map find-overlap)
         (filter identity)
         (count))))
