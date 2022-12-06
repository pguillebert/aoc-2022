(ns aoc2022.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def file "input5.txt")

(defn expand-moves
  [[cnt from to]]
   (repeat cnt [from to]))

(defn reduce-move-9000
  [stacks [from to]]
  (let [source-stack (nth stacks (dec from))
        value-moving (peek source-stack)
        source-stack-after (pop source-stack)
        dest-stack (nth stacks (dec to))
        dest-stack-after (conj dest-stack value-moving)
        ]
    (-> stacks
        (assoc (dec from) source-stack-after)
        (assoc (dec to) dest-stack-after))))

(defn reduce-move-9001
  [stacks [n from to]]
  (let [source-stack (nth stacks (dec from))
        values-moving (vec (take-last n source-stack))
        source-stack-after (vec (drop-last n source-stack))
        dest-stack (nth stacks (dec to))
        dest-stack-after (apply conj dest-stack values-moving)
        ]
    (-> stacks
        (assoc (dec from) source-stack-after)
        (assoc (dec to) dest-stack-after))))

(defn prep-data
  []
  (let [lines (-> (io/resource file)
                  (slurp)
                  (str/split #"\n")
                  )
        [stacks0 moves0] (split-with #(not (.isEmpty %)) lines)

        stacks (->> stacks0
                    (reverse) ; start from the bottom
                    (rest) ; skip the line with numbers labeling the stacks
                    (map #(str % " ")) ; add extra space for partition to work
                    (map #(partition 4 %))
                    (map (fn [line] (map second line))) ; only keep the letter, not the box
                    (vec)
                    (apply mapv vector) ;; transpose the matrix
                    (map (fn [line]
                           (remove #(= \space %) line))) ;; remove empty space
                    (map vec)
                    (vec))

        moves (->> moves0
                   (remove #(.isEmpty %))
                   (map #(str/split % #" "))
                   (map (fn [l] [(Integer/parseInt (nth l 1))
                                 (Integer/parseInt (nth l 3))
                                 (Integer/parseInt (nth l 5))])))]
    [stacks moves]))

(defn display [final-state]
  (->> final-state
       (map peek)
       (map str)
       (str/join)))

(defn day5-1
  "Uses a stack approach to apply one crate move at a time"
  []
  (let [[stacks moves] (prep-data)
        final-state (reduce reduce-move-9000 stacks (mapcat expand-moves moves))]
    (display final-state)))

(defn day5-2
  "crane can pick more than one crate at a time"
  []
  (let [[stacks moves] (prep-data)
        final-state (reduce reduce-move-9001 stacks moves)]
    (display final-state)))
