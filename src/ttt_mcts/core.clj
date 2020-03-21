(ns ttt-mcts.core
  (:require [ttt-mcts.tictactoe :as ttt]
            [ttt-mcts.mcts :as mcts]))

(def PLAYER-MCTS (ttt/create-player "MCTS" "T" #(mcts/run-mcts [%1 %2 %3] 2000)))

;(ttt/game-loop [ttt/RANDOM-PLAYER PLAYER-4] ttt/RANDOM-PLAYER)
(let [ps [ttt/HUMAN-PLAYER PLAYER-MCTS]
      first-p (rand-nth ps)]
  (ttt/game-loop ps first-p))

;(let [p1 ttt/RANDOM-PLAYER, p2 PLAYER-MCTS, ps [p1 p2]]
;  (->> (for [_ (range 100)] (ttt/game-loop ps
;                                          (if (= (rand-int 2) 0) p1 p2)
;                                          (ttt/create-board)
;                                          false))
;       (reduce #(assoc %1 (keyword %2) (inc ((keyword %2) %1 0))) {})
;       ((fn [coll]
;          (let [sum (apply + (vals coll))]
;            (reduce (fn [coll [k v]] (assoc coll k (float (/ v sum)))) {} coll))))
;       (sort)
;       (println)))