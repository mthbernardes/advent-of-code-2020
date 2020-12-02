(ns aoc2020.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]))

(def input-data (->> "day2_input" io/resource slurp string/split-lines))

(def policy-matrix {[true true]   false
                    [false true]  true
                    [true false]  true
                    [false false]  false})

(defn generate-checker-v1 [policy]
  (fn [password]
    (let [[raw-range letter] (string/split policy #" ")
          letter (keyword letter)
          [initial end] (->> (string/split raw-range #"-")
                             (map edn/read-string))
          policy-range (-> (range initial (+ 1 end)) set)
          amount-of-letter (->> (string/split password #"")
                                (group-by (comp identity keyword))
                                letter
                                count)]
      (contains? policy-range amount-of-letter))))

(defn generate-checker-v2 [policy]
  (fn [password]
    (let [[raw-range letter] (string/split policy #" ")
          password (string/split password #"")
          [initial end] (->> (string/split raw-range #"-") (map edn/read-string))
          result (conj [] (= (get  password (- initial 1)) letter)
                       (= (get password (- end 1)) letter))]
      (get policy-matrix result))))

(defn parse-line [line generate-checker-fn]
  (let [[policy password] (string/split line #": ")
        fn-checker (generate-checker-fn policy)]
    {:password password :policy policy :checker fn-checker}))

;part1
(->> input-data
     (map #(parse-line % generate-checker-v1))
     (map (fn [{:keys [checker password]}]
            (checker password)))
     frequencies)

;part2
(->> input-data
     (map #(parse-line % generate-checker-v2))
     (map (fn [{:keys [checker password]}]
            (checker password)))
     frequencies)