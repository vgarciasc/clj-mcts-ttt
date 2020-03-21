(ns ttt-mcts.mcts
  (:require [ttt-mcts.tictactoe :as ttt]
            [ttt-mcts.tree :as tree]))

(defn now [] (System/currentTimeMillis))

(defn make-data
  "Constructs the data of a game node tree"
  ([board last-player last-move players]
   (make-data board last-player last-move players 0 0))
  ([board last-player last-move players value visits]
   {:board       board
    :last-player last-player
    :last-move   last-move
    :players     players
    :value       value
    :visits      visits}))

(defn get-data
  [node & ks]
  (get-in node (apply vector :data ks)))

(defn is-move-expanded?
  [tree node move]
  (boolean (some #(= move (get-data % :last-move))
                 (tree/get-children tree node))))

(defn get-unexpanded-moves
  [tree node]
  (->> (ttt/get-valid-moves (get-data node :board))
       (remove #(is-move-expanded? tree node %))))

(defn is-expandable?
  [tree node]
  (if (ttt/is-there-victory? (get-data node :board))
    false
    (not (empty? (get-unexpanded-moves tree node)))))

(defn UCT
  [node parent]
  (let [exploitation (/ (get-data node :value) (get-data node :visits))
        exploration (Math/sqrt (* 2 (/ (Math/log (get-data parent :visits))
                                       (get-data node :visits))))]
    (+ exploitation exploration)))

(defn get-most-urgent-child
  [tree node]
  (->> (:children node)
       (map #(nth tree %))
       (map #(hash-map :node % :uct (UCT % node)))
       (sort-by :uct)
       (map :node)
       (last)))

(defn select
  [tree root]
  (if (or (tree/is-leaf? root) (is-expandable? tree root))
    root
    (select tree (get-most-urgent-child tree root))))

(defn expand
  [tree node]
  (let [{:keys [board last-player players]} (:data node)
        unexpanded-moves (get-unexpanded-moves tree node)]
    (if-not (empty? unexpanded-moves)
      (let [other-player (ttt/toggle-player last-player players)
            random-move (rand-nth unexpanded-moves)
            expanded-node (tree/create-node
                            (make-data (ttt/make-play board other-player random-move)
                                       other-player
                                       random-move
                                       players))]
        (tree/insert-node tree node expanded-node))
      tree)))

(defn simulate
  [node]
  (let [random-players (map #(assoc % :play-fn ttt/random-move)
                            (get-in node [:data :players]))]
    (ttt/game-loop
      random-players
      (ttt/toggle-player (get-data node :last-player)
                         random-players)
      (get-in node [:data :board])
      false)))

(defn backpropagate
  [tree node winner-icon]
  (if (nil? node)
    tree
    (let [{:keys [last-player players visits value]} (:data node)
          other-player (ttt/toggle-player last-player players)]
      (backpropagate (tree/update-node tree
                                       node
                                       (assoc (:data node)
                                         :visits (inc visits)
                                         :value (+ value (cond (= winner-icon (:icon last-player)) 1
                                                               (= winner-icon (:icon other-player)) -1
                                                               :else 0))))
                     (tree/get-parent tree node)
                     winner-icon))))

(defn run-mcts
  [[board last-player players] timeout]
  (let [start-time (now)]
    (loop [tree (tree/create-tree (make-data board last-player 0 players))
           iteration 0]
      ;(if (< iteration 500))
      (if (< (now) (+ start-time timeout))
        (let [selected (select tree (first tree))]
          (if (is-expandable? tree selected)
            (let [expanded-tree (expand tree selected)
                  expanded-node (last expanded-tree)
                  winner-icon (simulate expanded-node)]
              (recur (backpropagate expanded-tree expanded-node winner-icon)
                     (inc iteration)))
            (let [winner-icon (simulate selected)]
              (recur (backpropagate tree selected winner-icon)
                     (inc iteration)))))
        (get-data (get-most-urgent-child tree (first tree)) :last-move)))))