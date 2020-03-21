(ns ttt-mcts.mcts-test
  (:require [clojure.test :refer :all]
            [ttt-mcts.tree :refer :all]
            [ttt-mcts.mcts :refer :all]
            [ttt-mcts.tictactoe :as ttt]
            [ttt-mcts.utils :as u]))

(def pX (ttt/create-player "pX" "X" ttt/random-move))
(def pO (ttt/create-player "pO" "O" ttt/random-move))
(def ps [pX pO])

(deftest get-other-player-icon-test
  (are [p op] (= (ttt/toggle-player p ps) op)
              pX pO
              pO pX))

(deftest get-data-test
  (let [board (ttt/create-board ["X" "-" "-"
                                 "-" "X" "O"
                                 "-" "X" "O"])
        node (create-node (make-data board pX 0 ps))]
    (is (= (get-in node [:data :players]) ps))
    (is (= (get-data node :players) ps))))

(def game-tree [(create-node (make-data (ttt/create-board ["X" "-" "X"
                                                           "O" "O" "X"
                                                           "X" "O" "-"]) pX 0 ps -2 2) 0 [1 2])
                (create-node (make-data (ttt/create-board ["X" "O" "X"
                                                           "O" "O" "X"
                                                           "X" "O" "-"]) pO 1 ps 1 1) 1 [3])
                (create-node (make-data (ttt/create-board ["X" "-" "X"
                                                           "O" "O" "X"
                                                           "X" "O" "O"]) pO 8 ps 2 1) 2 [])
                (create-node (make-data (ttt/create-board ["X" "O" "X"
                                                           "O" "O" "X"
                                                           "X" "O" "X"]) pX 8 ps -1 1) 3 [])])

(deftest is-expandable?-test
  (is (= (is-expandable? game-tree (nth game-tree 0)) false))
  (is (= (is-expandable? game-tree (nth game-tree 1)) false))
  (is (= (is-expandable? game-tree (nth game-tree 2)) true))
  (is (= (is-expandable? game-tree (nth game-tree 3)) false))
  (let [tree [(create-node (make-data (ttt/create-board ["X" "-" "X"
                                                         "O" "O" "X"
                                                         "X" "O" "-"]) pO 0 ps 0 0) 0 [1])
              (create-node (make-data (ttt/create-board ["X" "-" "X"
                                                         "O" "O" "X"
                                                         "X" "O" "X"]) pX 0 ps 0 0) 0 [])]]
    (is (= (is-expandable? tree (nth tree 1)) false)))
  (let [tree [(create-node (make-data (ttt/create-board ["-" "O" "-"
                                                         "X" "O" "X"
                                                         "-" "O" "-"]) pX 0 ps 0 0) 0 [])]]
    (is (= (is-expandable? tree (nth tree 0)) false)))
  (let [tree [(create-node (make-data (ttt/create-board ["-" "O" "-"
                                                         "X" "O" "X"
                                                         "-" "O" "-"]) pO 0 ps 0 0) 0 [])]]
    (is (= (is-expandable? tree (nth tree 0)) false))))

(deftest is-move-expanded?-test
  (are [node-id move result] (= result
                                (is-move-expanded? game-tree
                                                   (nth game-tree node-id)
                                                   move))
                             0 1 true
                             0 8 true
                             0 3 false
                             0 2 false
                             1 3 false
                             2 1 false))

(deftest get-most-urgent-child-test
  (is (= (get-most-urgent-child game-tree (first game-tree))
         (nth game-tree 2)))
  (is (= (get-most-urgent-child game-tree (nth game-tree 1))
         (nth game-tree 3))))

(deftest select-test
  (is (= (select game-tree (first game-tree))
         (nth game-tree 2)))
  (let [tree [(create-node (make-data (ttt/create-board ["X" "-" "X" "O" "O" "X" "X" "O" "-"]) pX 0 ps -2 2) 0 [1 2])
              (create-node (make-data (ttt/create-board ["X" "O" "X" "O" "O" "X" "X" "O" "-"]) pO 1 ps 1 1) 1 [3])
              (create-node (make-data (ttt/create-board ["X" "-" "X" "O" "O" "X" "X" "O" "O"]) pO 8 ps 0.9 1) 2 [])
              (create-node (make-data (ttt/create-board ["X" "O" "X" "O" "O" "X" "X" "O" "X"]) pX 8 ps -1 1) 3 [])]]
    (is (= (select tree (first tree))
           (nth tree 3)))))

(deftest expand-test
  ;(is (= (expand game-tree (nth game-tree 3))
  ;       game-tree))
  (is (= (expand game-tree (nth game-tree 1))
         game-tree))
  (is (= (expand game-tree (nth game-tree 2))
         [(create-node (make-data (ttt/create-board ["X" "-" "X" "O" "O" "X" "X" "O" "-"]) pX 0 ps -2 2) 0 [1 2])
          (create-node (make-data (ttt/create-board ["X" "O" "X" "O" "O" "X" "X" "O" "-"]) pO 1 ps 1 1) 1 [3])
          (create-node (make-data (ttt/create-board ["X" "-" "X" "O" "O" "X" "X" "O" "O"]) pO 8 ps 2 1) 2 [4])
          (create-node (make-data (ttt/create-board ["X" "O" "X" "O" "O" "X" "X" "O" "X"]) pX 8 ps -1 1) 3 [])
          (create-node (make-data (ttt/create-board ["X" "X" "X" "O" "O" "X" "X" "O" "O"]) pX 1 ps 0 0) 4 [])])))

(deftest simulate-test
  (is (= (simulate (nth game-tree 1)) "O"))
  (is (= (simulate (nth game-tree 2)) "X"))
  (is (= (set (for [_ (range 20)] (simulate (nth game-tree 0))))
         #{"X" "O"})))

(deftest backpropagate-test
  (is (= (backpropagate game-tree (nth game-tree 2) "X")
         [(-> (nth game-tree 0)
              (update-in [:data :visits] inc)
              (update-in [:data :value] inc))
          (nth game-tree 1)
          (-> (nth game-tree 2)
              (update-in [:data :visits] inc)
              (update-in [:data :value] dec))
          (nth game-tree 3)])))

(deftest run-mcts-test
  (is (= ()))
  (is (= (let [results (for [_ (range 10)]
                         (run-mcts [["X" "O" "O"
                                     "-" "X" "-"
                                     "-" "-" "O"] pO ps] 500))]
           (set results))
         #{5})))
