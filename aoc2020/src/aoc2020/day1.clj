(ns aoc2020.day1
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def input (->> "input" io/resource slurp string/split-lines (map edn/read-string)))

(def test-data [1721 979 366 299 675 1456])

;part 1
(defn subset [number set-of-numbers]
  (->> set-of-numbers
       (map #(if (not (= number %))
               (conj [] number %)))
       (filter identity)))

(defn sum-equal-2020 [values]
  (let [sum (reduce + values)]
    (when (= 2020 sum) values)))

(->> (map #(subset % input) input)
     (reduce concat)
     (map sum-equal-2020)
     (filter identity))

;; part 2

(defn subset-three [number-couple set-of-numbers]
  (->> set-of-numbers
       (map (fn [number]
              (when-not (contains? number-couple number)
                (apply sorted-set (conj number-couple number)))))))

(defn all-combinations [input-data]
  (->> input-data
       (map (fn [number]
              (map #(when-not (= number %)
                      (apply sorted-set (conj #{} number %)))
                   input-data)))
       (reduce concat)
       (filter identity)))

(defn find-it [input-data]
  (->> input-data
       all-combinations
       (map #(subset-three % input-data))
       flatten
       (filter identity)
       set
       (map sum-equal-2020)
       (filter identity)))

(find-it test-data)