(ns aoc2022.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(def file "input8.txt")

(defn scan-mark [list]
  (second
   (reduce (fn [[max-seen acc] tree]
             (if (> tree max-seen)
               [tree (conj acc {:height tree :visible true})]
               [max-seen (conj acc {:height tree :visible false})]
               ))
           [-1 []]
           list)))

(defn visibility-merge
  [v1 v2]
    (if (and (boolean? v1) (boolean? v2))
      (or v1 v2)
      ;; non boolean: it's the :height key, same value expected. pick v2
      v2))

(defn merge-one-tree [mergefunc & tree-maps]
  (assert (apply = (map :height tree-maps)) "different trees heights?!")
  (apply merge-with
         mergefunc
         tree-maps))

(defn merge-forests [mergefunc & forests]
  (apply map
   (fn [& lines]
     (apply map (partial merge-one-tree mergefunc) lines))
   forests))

(defn transpose [m] (apply mapv vector m))

(defn process-visible
  [merged-data]
  (->> merged-data
       (map (fn [line] (map :visible line)))
       (flatten)
       (filter identity)
       (count)))

(defn mk-score [idx list]
  (let [our-tree-height (nth list idx)
        to-the-left (take idx list)
        smaller-trees (->> to-the-left
                           (reverse)
                           (reduce (fn [acc height]
                                     (if (< height our-tree-height)
                                       (conj acc height)
                                       ; if stopped by a tree add one item
                                       (reduced (conj acc :extra))))
                                   []))]
    (count smaller-trees)))

(defn scenic-score [list]
  (second
   (reduce
    (fn [[idx acc] tree]
      [(inc idx) (conj acc {:height tree
                            :score (mk-score idx list)})])
    [0 []]
    list)))

(defn scenic-merge [v1 v2]
  ;; note that this will also square the :height value, YOLO
  (* v1 v2))

(defn process-scenic
  [merged-data]
  (->> merged-data
       (map (fn [line] (map :score line)))
       (flatten)
       (apply max)))

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
                  (vec))]
    data))

(defn day8 [mark-function merge-function process-function]
  (let [data (day8-data)
;        [[3 0 3 7 3]
;         [2 5 5 1 2]
;         [6 5 3 3 2]
;         [3 3 5 4 9]
;         [3 5 3 9 0]]

        left-fun mark-function
        right-fun (comp reverse mark-function reverse)
        top-fun mark-function
        bottom-fun (comp reverse mark-function reverse)

        left-matches (map left-fun data)
        right-matches (map right-fun data)
        top-matches (transpose (map top-fun (transpose data)))
        bottom-matches (transpose (map bottom-fun (transpose data)))

        merged-data (merge-forests merge-function
                                   left-matches right-matches top-matches bottom-matches)
        ]
    (process-function merged-data)))
