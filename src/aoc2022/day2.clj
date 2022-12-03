(ns aoc2022.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def file "input2.txt")

(defn get-data
  []
  (let [fcontent (slurp (io/resource file))
        plays (map #(str/split % #" ") (str/split fcontent #"\n"))
        ]

    (->> plays
      (map (fn [play]
             (map #(first (char-array %)) play))))))

(defn score-play
  [[opponent you]]
  (let [intopp (int opponent)
        intyou (- (int you) 23)
        delta (- intopp intyou)
        winner (mod delta 3) ; 2 means you win, 1 you lose
        winpoints (case winner
                    0 3
                    1 0
                    2 6)
        result (+ winpoints (- intyou 64))
        ]
    (println (str (char intopp) " --- " (char intyou) " -- " winner " " result))

    result))

(defn strategize
  [[opponent wanted]]
  (let [intopp (int opponent)
        intwanted (- (int wanted) 88) ; 0 is lose, 1 is draw, 2 is win
        toplay0 (mod (+ intopp intwanted) 3)
        toplay (+ toplay0 65)
        ]
    (println (str (char intopp) " --- " (char wanted) " -- " intwanted "  >> " (char toplay)))

    [(char intopp) (char (+ 23 toplay))]))

(defn day2-1
  []
  (->> (get-data)
;       (take 20)
       (map score-play)
       (reduce +)))

(defn day2-2
  []
  (->> (get-data)
;       (take 20)
       (map strategize)
       (map score-play)
       (reduce +)))

