(ns aoc2022.day6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def file "input6.txt")

(defn find-all-different [nbchars tup]
  (= nbchars (count (into #{} tup))))

(defn find-pred-pos [pred coll]
  (->> coll
       (keep-indexed (fn [index item]
                      (when (pred item) index)))
       (first)))

(defn day6
  [nbchars]
  (let [data (-> (io/resource file)
                 (slurp))
        windows (partition nbchars 1 data)
        pos (find-pred-pos (partial find-all-different nbchars) windows)
        ]
    (+ pos nbchars)))
