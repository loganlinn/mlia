(ns ch03.tree)

(defn calc-shannon-ent [data]
  (let [label-freq (frequencies (map last data))
        n (float (count data))]
    (reduce (fn [ent freq]
              (let [prob (/ freq n)]
                (- ent (* prob (Math/log prob))))) ;; TODO log base 2
            0.0
            (vals label-freq))))

(comment
  (calc-shannon-ent [[1 1 "yes"] [1 1 "yes"] [1 0 "no"] [0 1 "no"] [0 1 "no"]])
  (calc-shannon-ent [[1 1 "maybe"] [1 1 "yes"] [1 1 "yes"] [1 0 "no"] [0 1 "no"] [0 1 "no"]])
  )

(defn split-data [data axis value]
  (reduce (fn [coll row]
            (if (= (nth row axis) value)
              (conj coll (vec (concat (subvec row 0 axis)
                                      (subvec row (inc axis)))))
              coll))
          (empty data)
          data))

(comment
  (split-data [[1 1 "yes"] [1 1 "yes"] [1 0 "no"] [0 1 "no"] [0 1 "no"]] 0 1)
  (split-data [[1 1 "yes"] [1 1 "yes"] [1 0 "no"] [0 1 "no"] [0 1 "no"]] 2 "yes")
  )

(defn choose-best-feature-to-split [data]
  (let [n (float (count data))
        ent (calc-shannon-ent data)
        gain-diff (fn [i distinct-vs]
                    (->> (for [v distinct-vs
                               :let [split (split-data data i v)]]
                           (* (/ (count split) n)
                              (calc-shannon-ent split)))
                         (reduce + 0)
                         (- ent)))
        gains (apply map (fn [i & vs]
                           [i (gain-diff i (distinct vs))])
                     (range)
                     (map butlast data))]
    (first (apply max-key second gains))))

(comment
  (choose-best-feature-to-split [[1 1 "yes"] [1 1 "yes"] [1 0 "no"] [0 1 "no"] [0 1 "no"]])
  )

(defn majority-cnt [classes]
  (->> (frequencies classes)
       (apply max-key val)
       (key)))

(defn occurances [coll v]
  (reduce #(if (= %2 v) (inc %1) %1) 0 coll))

(defn create-tree [data labels]
  (let [classes (map last data)
        ]))
