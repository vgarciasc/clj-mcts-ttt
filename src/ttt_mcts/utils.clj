(ns ttt-mcts.utils)

(defn find-first
  [& more]
  (first (apply filter more)))

(defn seq-contains?
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))