(ns ttt-mcts.tree-test
  (:require [clojure.test :refer :all]
            [ttt-mcts.tree :refer :all]))

(deftest create-node-test
  (let [data [1 2 3]
        id 230
        children [3]
        node {:data     data
              :id       id
              :children [3]}]
    (is (= node (create-node data id children)))))

(deftest create-tree-test
  (let [data [1 2 3]]
    (is (= [(create-node data 0)]
           (create-tree data)))))

(deftest insert-node-test
  (let [root (create-node "A" 0)
        tree [root
              (create-node "B" 1)
              (create-node "C" 2)]
        child (create-node "D" -1)]
    (is (= [(create-node "A" 0 [3])
            (create-node "B" 1)
            (create-node "C" 2)
            (create-node "D" 3)]
           (insert-node tree root child)))))

(deftest update-node-test
  (let [node_0 (create-node "A" 0)
        node_1 (create-node "B" 1)
        node_2 (create-node "C" 2)
        tree [node_0 node_1 node_2]]
    (is (= [(create-node "Z" 0) node_1 node_2]
           (update-node tree node_0 "Z")))
    (is (= [(create-node "Z" 0)
            (create-node "X" 1)
            (create-node "Y" 2)]
           (-> (update-node tree node_0 "Z")
               (update-node node_1 "X")
               (update-node node_2 "Y"))))))

(deftest get-parent-test
  (let [node_0 (-> (create-node "A" 0)
                   (assoc :children [1]))
        node_1 (-> (create-node "B" 1)
                   (assoc :children [2]))
        node_2 (create-node "C" 2)
        tree [node_0 node_1 node_2]]
    (are [node parent] (= (get-parent tree node) parent)
                       node_0 nil
                       node_1 node_0
                       node_2 node_1)))

(deftest get-children-test
  (let [node_0 (create-node "A" 0 [1 2])
        node_1 (create-node "B" 1)
        node_2 (create-node "C" 2)
        tree [node_0 node_1 node_2]]
    (are [node children] (= (get-children tree node) children)
                         node_0 [node_1 node_2]
                         node_1 []
                         node_2 [])))

(deftest is-leaf?-test
  (let [node_0 (-> (create-node "A" 0)
                   (assoc :children [1]))
        node_1 (-> (create-node "B" 1)
                   (assoc :children [2]))
        node_2 (create-node "C" 2)]
    (are [node parent] (= (is-leaf? node) parent)
                       node_0 false
                       node_1 false
                       node_2 true)))

(deftest is-root?-test
  (let [node_0 (-> (create-node "A" 0)
                   (assoc :children [1]))
        node_1 (-> (create-node "B" 1)
                   (assoc :children [2]))
        node_2 (create-node "C" 2)
        tree [node_0 node_1 node_2]]
    (are [node b] (= (is-root? tree node) b)
                  node_0 true
                  node_1 false
                  node_2 false)))

(deftest is-node?-test
  (are [node result] (= (is-node? node) result)
                     {:id 0 :data "A" :children []} true
                     {:id 0 :data "A" :children [1 2]} true
                     {:id 1 :children []} true
                     {:children []} false
                     {:id 3} false
                     {:id 0 :data "A" :not-key "C"} false
                     {:id 0 :data "A" :not-key "C" :children [2 3]} true))
