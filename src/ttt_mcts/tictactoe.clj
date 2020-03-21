(ns ttt-mcts.tictactoe
  (:require [clojure.string :as s]))

(def DEFAULT-TILE "-")

; RENDERING

(defn row-start [row] (* row 3))

(defn create-board
  "Creates a tic-tac-toe board"
  ([board] board)
  ([] (vec (repeat 9 DEFAULT-TILE))))

(defn render-row-elem
  "Renders a single element of the board"
  [pos elem]
  (let [elem-str (if (not= elem DEFAULT-TILE) elem (nth ["₀" "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉"] pos))]
    (case (mod pos 3)
      (0 1) (str elem-str "|")
      (2) (str elem-str))))

(defn render-row-board
  "Renders a row of the board"
  [board row-num]
  (loop [col 0 res ""]
    (if (< col 3)
      (let [pos (+ col (row-start row-num))]
        (recur
          (inc col)
          (str res (render-row-elem pos (nth board pos)))))
      res)))

(defn print-board
  "Renders the board in a pretty manner"
  [board turn player]
  (println "======" "Turn" (s/join "#" (str turn))
           "- Player" (:name player) "(" (:icon player) ")")
  (doseq [row (range 0 3)]
    (println (render-row-board board row)))
  (println "======"))

; PLAYING

(defn is-space-empty?
  "Checks if space is empty"
  [board pos]
  (= (nth board pos) DEFAULT-TILE))

(defn is-valid-play?
  "Checks if play is valid"
  [board pos]
  (and
    (integer? pos)
    (< pos 9) (>= pos 0)
    (is-space-empty? board pos)))

(defn make-play
  "Executes player move in board"
  [board player pos]
  (assoc board pos (:icon player)))

(defn get-input
  "Gets string from input stream"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))

(defn prompt-move
  "Prompts player to make a move"
  [board player & _]
  (println (str (:name player) ", please select a position."))
  (let [pos (Integer/parseInt (get-input))]
    (if (is-valid-play? board pos)
      pos
      (do
        (println "That isn't a valid position. Please try again.")
        (prompt-move board player)))))

(defn is-seq-victory?
  "Checks if a certain sequence causes the game to end"
  [seq]
  (if (apply = seq) (first seq) nil))

(defn is-row-victory?
  [board row-num]
  (let [row (nth (partition 3 board) row-num)]
    (is-seq-victory? row)))

(defn is-col-victory?
  [board col-num]
  (let [col (take-nth 3 (drop col-num board))]
    (is-seq-victory? col)))

(defn get-diag
  [board diag-num]
  (case diag-num
    0 [(nth board 0) (nth board 4) (nth board 8)]
    1 [(nth board 2) (nth board 4) (nth board 6)]
    nil))

(defn is-diag-victory?
  [board diag-num]
  (is-seq-victory? (get-diag board diag-num)))

(defn get-victory
  [board]
  (let [victors (list (reduce #(conj %1 (is-row-victory? board %2)) [] (range 0 3))
                      (reduce #(conj %1 (is-col-victory? board %2)) [] (range 0 3))
                      (reduce #(conj %1 (is-diag-victory? board %2)) [] (range 0 2)))]
    (->> victors
         (flatten)
         (filter #(not (nil? %)))
         (first))))

(defn is-there-victory?
  [board]
  (let [winner (get-victory board)]
    (and (not= winner nil) (not= winner DEFAULT-TILE))))

(defn get-valid-moves
  [board]
  (filter #(is-valid-play? board %) (range 0 9)))

(defn are-there-valid-moves?
  [board]
  (not (empty? (get-valid-moves board))))

(defn toggle-player
  [player players]
  (first (filter #(not= (:icon %) (:icon player)) players)))

;; PLAYER HANDLING

(defn hash-param
  [& params]
  (reduce #(assoc %1 (name %2) %2) {} params))

(defn create-player
  "Generates a new player"
  [name icon play-fn]
  {:name    name :icon icon
   :play-fn play-fn})

;; LOOP

(defn game-loop
  ([players initial-player]
   (game-loop players initial-player (create-board)))
  ([players initial-player initial-board]
   (game-loop players initial-player initial-board true))
  ([players initial-player initial-board verbose]
   (loop [board initial-board
          curr-player initial-player
          turn 1]
     (let [winner (get-victory board)]
       (if (is-there-victory? board)
         (do (when verbose (println "Game over. Winner is " winner "!")) winner)
         (if (not (are-there-valid-moves? board))
           (do (when verbose (println "Game over. It's a tie!")) DEFAULT-TILE)
           (let [new-board (make-play board curr-player
                                      ((:play-fn curr-player) board curr-player players))]
             (when verbose (print-board new-board turn curr-player))
             (recur new-board (toggle-player curr-player players) (inc turn)))))))))

;; MAIN

(defn random-move
  [board & _]
  (->> (range 0 9)
       (filter #(is-valid-play? board %))
       (rand-nth)))

(def HUMAN-PLAYER (create-player "Human" "H" prompt-move))
(def RANDOM-PLAYER (create-player "Random" "R" random-move))