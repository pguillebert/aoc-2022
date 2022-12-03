(ns aoc2022.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def file "input1.txt")

(defn get-data
  []
  (let [fcontent (slurp (io/resource file))
        tokens (str/split fcontent #"\n")]

    (->> tokens
         (partition-by #(= "" %))
         (remove #(= % '("")))
         (map (fn [elf]
                (->> elf
                     (map #(Integer/parseInt %))
                     (reduce +))))
         )))


(defn day1-1
  []
  (->> (get-data)
       (apply max)))

(defn day1-2
  []
  (->> (get-data)
       (sort >)
       (take 3)
       (apply +)
       ))
