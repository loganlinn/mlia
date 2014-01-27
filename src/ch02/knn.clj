(ns ch02.knn
  (:require [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn sq [a] (* a a))

(defn distance [a b]
  (->> (map (comp sq -) b a)
       (reduce +)
       (Math/sqrt)))

(comment
  (= (distance [1 0 0 1] [7 6 9 4])
     (Math/sqrt (+ 36 36 81 9))))

(defn classify0 [data input k]
  (->> data
       (map (fn [[vs label]] [label (distance input vs)]))
       (sort-by second)
       (take k)
       (map first)
       (frequencies)
       (apply max-key val)
       (first)))

(comment
  (let [data [[[3 104] :romance]
              [[2 100] :romance]
              [[1 81] :romance]
              [[101 10] :action]
              [[99 5] :action]
              [[98 2] :action]]]
    (classify0 data [18 90] 3)))

(defn test-set [filename]
  (->> (io/resource filename)
       (io/reader)
       (line-seq)
       (map #(map read-string (string/split % #"\s+")))))

