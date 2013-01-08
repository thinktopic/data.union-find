(ns jordanlewis.data.union-find-perf
  (:use jordanlewis.data.union-find
        criterium.core))

(defn union-conj-test []
  (apply union-find (range 100000)))

(defn union-bench []
  (-> (union-find 1 2 3 4 5 6 7 8)
      (union 1 2)
      (union 3 4)
      (union 5 6)
      (union 7 8)
      (union 2 3)
      (union 5 7)
      (union 1 4)))

(bench union-bench)
