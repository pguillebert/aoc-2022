(ns aoc2022.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(def file "input8.txt")

(defn scan-mark [dir list]
  (second
   (reduce (fn [[max-seen acc] tree]
             (if (> tree max-seen)
               [tree (conj acc {:item tree :visible true :direction dir})]
               [max-seen (conj acc {:item tree :visible false})]
               ))
           [-1 []]
           list)))

(defn merge-one-tree [& tree-maps]
  (assert (apply = (map :item tree-maps)) "different trees heights?!")
  (apply merge-with
         (fn [v1 v2]
           (if (and (boolean? v1) (boolean? v2))
             (or v1 v2)
             ; non boolean: either :item (same value expected)
             ; or :direction (not useful for final processing). pick v2
             v2))
         tree-maps))

(defn merge-forests [& forests]
  (apply map
   (fn [& lines]
     (apply map merge-one-tree lines))
   forests))

(defn transpose [m] (apply mapv vector m))

(defn day8-data
  []
  (let [data0 (-> (io/resource file)
                  (slurp)
                  (str/split #"\n"))
        data (->> data0
                  (map (fn [s]
                         (vec
                          (map #(Integer/parseInt (str %))
                               (seq s)))))
                  (vec))

        minidata (->> data
                      (take 6)
                      (map #(take 6 %)))

        left-fun (comp identity (partial scan-mark :left) identity)
        right-fun (comp reverse (partial scan-mark :right) reverse)
        top-fun (comp identity (partial scan-mark :top) identity)
        bottom-fun (comp reverse (partial scan-mark :bottom) reverse)

        left-matches (map left-fun data)
        right-matches (map right-fun data)
        top-matches (transpose (map top-fun (transpose data)))
        bottom-matches (transpose (map bottom-fun (transpose data)))

        merged-data (merge-forests left-matches right-matches top-matches bottom-matches)

        visible (map (fn [line] (map :visible line)) merged-data)]

    (->> visible
         (flatten)
         (filter identity)
         (count))
    ))
