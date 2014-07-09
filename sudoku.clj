;; http://iloveponies.github.io/120-hour-epic-sax-marathon/sudoku.html

(ns sudoku-solver
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def invalid-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 1 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(all-values 1)

(defn value-at [board coordinates]
  (get-in board coordinates))


(value-at sudoku-board [0 1]) ;=> 3
(value-at sudoku-board [0 0]) ;=> 5

(defn has-value? [board coordinates]
  (not (zero? (value-at board coordinates))))

(has-value? sudoku-board [0 0]) ;=> true
(has-value? sudoku-board [0 2]) ;=> false


(defn row-values [board [x _]]
   (set (get board x)))

(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(defn col-values [board [_ y]]
  (set (map
         (fn [b] (get b y))
         board)))

(col-values sudoku-board [0 2]) ;=> #{0 8}
(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}


(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(coord-pairs [0 1])   ;=> [[0 0] [0 1]
                      ;    [1 0] [1 1]]

(coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
                      ;    [1 0] [1 1] [1 2]
                      ;    [2 0] [2 1] [2 2]]

(defn top-left [coords]
    (map (fn [i] (* (int (/ i 3)) 3)) coords))

(defn block-pairs [coords]
  (let [[x y] (top-left coords)]
    (for [[x' y'] (coord-pairs [0 1 2])]
      [(+ x' x) (+ y' y)])))

(defn block-values [board coords]
  (set(map (fn [args] (value-at board args))
           (block-pairs coords))))

(top-left [0 2])
(top-left [4 2])
(top-left [8 8])
(top-left [4 5])


(block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
(block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}

((fn [x] x) 10)
(#(identity %) 10)
(#(* %1 %2) 2 3)

(defn valid-values-for [board [x y]]
  (if (has-value? board [x y])
    #{}
    (set/difference all-values (set/union (block-values board [x y])
                                          (col-values board [x y])
                                          (row-values board [x y])))))

(valid-values-for sudoku-board [0 0]) ;=> #{}
(valid-values-for sudoku-board [0 2]) ;=> #{1 2 4})

(defn filled? [board]
    (not (some #{0} (flatten board)))
)

(filled? sudoku-board)
(filled? solved-board)


(defn rows [board]
  (for [x (range 0 9)] (row-values board [x 0])))

(rows sudoku-board) ;=> [#{5 3 0 7}
                    ;    #{6 0 1 9 5}
                    ;    #{0 9 8 6}
                    ;    #{8 0 6 3}
                    ;    #{4 0 8 3 1}
                    ;    #{7 0 2 6}
                    ;    #{0 6 2 8}
                    ;    #{0 4 1 9 5}
                    ;    #{0 8 7 9}]

(rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]


(defn cols [board]
  (for [y (range 0 9)] (col-values board [0 y])))

(cols sudoku-board) ;=> [#{5 6 0 8 4 7}
                    ;    #{3 0 9 6}
                    ;    #{0 8}
                    ;    #{0 1 8 4}
                    ;    #{7 9 0 6 2 1 8}
                    ;    #{0 5 3 9}
                    ;    #{0 2}
                    ;    #{0 6 8 7}
                    ;    #{0 3 1 6 5 9}]

(cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]

(defn blocks [board]
  (for [x (range 0 9 3) y (range 0 9 3)]
    (block-values board [x y])
  ))

(blocks sudoku-board) ;=> [#{5 3 0 6 9 8}
                      ;    #{0 7 1 9 5}
                      ;    #{0 6}
                      ;    #{8 0 4 7}
                      ;    #{0 6 8 3 2}
                      ;    #{0 3 1 6}
                      ;    #{0 6}
                      ;    #{0 4 1 9 8}
                      ;    #{2 8 0 5 7 9}]

(defn valid-set? [items]
  (and (not (#{0} items))
       (= (count items) 9)))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(valid-blocks? solved-board)  ;=> truthy
(valid-blocks? invalid-board)

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(valid-rows? solved-board)  ;=> truthy
(valid-rows? invalid-board)

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(valid-cols? solved-board)  ;=> truthy
(valid-cols? invalid-board)

 (defn valid-solution [board]
   (and (valid-blocks? board)
        (valid-cols? board)
        (valid-rows? board)))

 (valid-solution solved-board)
