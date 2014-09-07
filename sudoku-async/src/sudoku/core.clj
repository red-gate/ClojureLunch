(ns sudoku.core
  (:require [clojure.core.async :refer [go go-loop chan <! >! >!! <!!]]))

(defn get-missing [x]
  (- (reduce + (range 1 5)) (reduce + x)))

(defn square [x y elim done]
  (go-loop [eliminated #{}]
     (let [n (<! elim)
           new-eliminated (conj eliminated n)]
       (when (and (not (contains? eliminated n)) (= (count new-eliminated) 3))
         (>!! done {:x x :y y :result (get-missing new-eliminated)}))
       (recur new-eliminated))))

(defn range-excluding-item
  ([item end] (range-excluding-item item 0 end))
  ([item start end] (remove #(= % item) (range start end))))

(defn get-item [x y coll]
  (get-in coll [y x]))

(defn eliminate-vertical [x y n board-channels]
  (doall
   (map #(>!! (get-item x % board-channels) n) (range-excluding-item y 4))))

(defn eliminate-horizontal [x y n board-channels]
  (doall
   (map #(>!! (get-item % y board-channels) n) (range-excluding-item x 4))))

(defn eliminate-quadrant [x y n board-channels]
  (let [sX (* (quot x 2) 2)
        sY (* (quot y 2) 2)]
    (doseq [i (range sX (+ sX 2))
            j (range sY (+ sY 2))]
      (when (and (not= i x) (not= j y))
        (>!! (get-item i j board-channels) n)))))

(defn eliminate [x y n board-channels]
  (go
    (eliminate-horizontal x y n board-channels)
    (eliminate-vertical x y n board-channels)
    (eliminate-quadrant x y n board-channels)))

(defn setup [board board-channels]
  (go
   (doseq [x (range 4)
           y (range 4)]
    (let [n (get-item x y board)]
      (when-not (= n 0)
        (doall
         (map #(>!! (get-item x y board-channels) %) (range-excluding-item n 1 5))))))))

(defn spawn-squares [board-channels done-channel]
  (doseq [x (range 4)
          y (range 4)]
    (square x y (get-item x y board-channels) done-channel)))

(defn finished?
  [solved-board]
  (not (some #{0} (flatten solved-board))))

(defn setup-channels []
  (letfn [(channel-row []
            (into [] (repeatedly 4 #(chan))))]
    (into [] (repeatedly 4 #(channel-row)))))

(defn solve [sudoku-board]
  (let
     [sudoku-board-channels (setup-channels)
      done (chan)]
  (spawn-squares sudoku-board-channels done)
  (setup sudoku-board sudoku-board-channels)
  (loop [solved-board sudoku-board]
   (if (finished? solved-board)
     solved-board
     (let [{y :y x :x n :result} (<!! done)]
       (eliminate x y n sudoku-board-channels)
       (recur (assoc-in solved-board [y x] n)))))))

(println (solve
          [[0 0 0 0]
           [3 1 4 2]
           [2 4 3 1]
           [0 0 0 0]]))



