(ns ttt-mcts.tree
  (:require [ttt-mcts.utils :as u]
            [clojure.test :refer :all]))

(defn create-node
  ([data]
   (create-node data -1))
  ([data id]
   (create-node data id []))
  ([data id children]
   {:data data :children children :id id}))

(defn is-node? [m]
  (boolean (and (:id m) (:children m))))

(defn create-tree [data]
  (vector (create-node data 0)))

(defn insert-node
  [tree parent node]
  {:pre [(is (is-node? parent))
         (is (is-node? node))]}
  (let [child (assoc node :id (count tree))
        parent-id (:id parent)
        new-tree (conj tree child)]
    (assoc-in new-tree
              [parent-id :children]
              (conj (:children (nth new-tree parent-id)) (count tree)))))

(defn update-node
  [tree node new-data]
  (assoc-in tree [(:id node) :data] new-data))

(defn get-parent
  [tree node]
  (let [child-id (:id node)]
    (first (filter #(some #{child-id} (:children %)) tree))))

(defn get-children
  [tree node]
  (filter #(u/seq-contains? (:children node) (:id %)) tree))

(defn is-leaf?
  [node]
  (empty? (:children node)))

(defn is-root?
  [tree node]
  (nil? (get-parent tree node)))