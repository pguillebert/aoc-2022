(ns aoc2022.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def file "input3.txt")


(defn split-middle
  [str]
  (let [strlen (count str)
        f (subs str 0 (/ strlen 2))
        l (subs str (/ strlen 2))]
[f l]))

(defn common-letter
  [groups]
  (let [existence (map #(into #{} (map str %))
                       groups)
        intersection (apply set/intersection existence)]
    ;; assume only one common char among the groups.
    ;; second first is to take the 1st char
    (ffirst intersection)))

(defn mk-prio
  [chara]
  (if (Character/isUpperCase chara)
    (- (int chara) 38)
    (- (int chara) 96)))

(defn day3-1
  []
  (let [fcontent (slurp (io/resource file))
        rucksacks (str/split fcontent #"\n")]

    (->> rucksacks
         (map split-middle)
;         (take 4)
         (map common-letter)
         (map mk-prio)
         (reduce +)
         )))


(defn mk-groups
  [coll]
  (->> (map-indexed vector coll)
       (partition-by (fn [[idx item]] (quot idx 3)))
       (map #(map second %))))

(defn day3-2
  []
  (let [fcontent (slurp (io/resource file))
        rucksacks (str/split fcontent #"\n")]

    (->> rucksacks
;         (take 9)
         mk-groups
         (map common-letter)
         (map mk-prio)
         (reduce +))))

