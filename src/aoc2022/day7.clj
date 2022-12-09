(ns aoc2022.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]))

(def file "input7.txt")

(defn mktree [tree breadcrumb response]
  (if (empty? breadcrumb)
    tree    ;; do not store a nil key for the root
    (reduce
     (fn [tree line]
       (update-in tree breadcrumb
                  (fn [existing-direntry]
                    ; (println "existing-direntry" existing-direntry)
                    (let [[type name] (str/split line #" ")]
                      (if (= type "dir")
                        (assoc existing-direntry name {})
                        (assoc existing-direntry name (Integer/parseInt type)) ;; file
                        )))))
     tree
     response)))


(defn dirsize
  ([tree path]
   (dirsize (get-in tree path)))

  ([dir]
   (reduce (fn [acc [name value]]
             (+ acc (if (map? value)
                      (dirsize value)
                      value)))
           0
           dir)))

(defn walk-tree-dir
  ([tree]
   (walk-tree-dir [[][]] tree))

  ([[path acc] tree]
  (let [[path acc]
    (reduce
     (fn [[path acc] [name value]]
       (if (map? value)
         (walk-tree-dir [(conj path name) acc] value)
         [path acc]))
     [path acc]
     tree)
        ]

    (if (empty? path) ;; if we traversed all
      (conj acc []) ;; return acc, and add the root selector []
      [(pop path) (conj acc path)] ;; else go up one level
        ))))


(defn day7-data
  []
  (let [data (->(io/resource file)
                (slurp)
              ;test-data
              (str/split #"\n"))

        command-response (->> data
                (partition-by #(.startsWith % "$"))
                (map-indexed #(if (even? %1)
                                (interpose '() (map list %2))
                                (list %2)))
                (apply concat)
                (partition 2)
                (map (fn [[a b]]
                       [(first a) b])))

        bread-and-tree (reduce (fn [[breadcrumb tree] [command response]]
                                        ; (println breadcrumb)
                                 (cond
                                   (.startsWith command "$ cd /") [[] tree]
                                   (.startsWith command "$ cd ..") [(pop breadcrumb) tree]
                                   (.startsWith command "$ cd")
                                   (let [newdir (second (str/split command #"cd "))]
                                     [(conj breadcrumb newdir) tree])
                                   (= command "$ ls")
                                   [breadcrumb (mktree tree breadcrumb response)]
                                   ))
                               [[]{}]
                               command-response)

        tree (second bread-and-tree)
        walk (walk-tree-dir tree)
        all-sizes (map (fn [path] [path (dirsize tree path)])
                       walk)]
    all-sizes))

(defn day7-1 []
  (->> (day7-data)
       (filter (fn [[path size]] (< size 100000)))
       (map second)
       (reduce +)))

(defn day7-2 []
  (let [data (into {} (day7-data))
        used-space (get data [])
        freespace (- 70000000 used-space)
        tofree (- 30000000 freespace)
        ]
    (println used-space)
    (println tofree)

    (->> (day7-data)
         (map second)
         (sort)
         (drop-while #(< % tofree))
         (first))))
