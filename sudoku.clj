;; http://iloveponies.github.io/120-hour-epic-sax-marathon/sudoku.html

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

(def all-values #{1 2 3 4 5 6 7 8 9})

(all-values 1)

(defn value-at [board coordinates]
  (get-in board coordinates)
  )


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
         board))
  )

(col-values sudoku-board [0 2]) ;=> #{0 8}
(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}


(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]
    )
  )
(coord-pairs [0 1])   ;=> [[0 0] [0 1]
                      ;    [1 0] [1 1]]

(coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
                      ;    [1 0] [1 1] [1 2]
                      ;    [2 0] [2 1] [2 2]]


(defn top-left [coords]
    (map (fn [i] (* (int (/ i 3)) 3)) coords
  )
 )

(defn block-pairs [coords]
  (let [[x y] (top-left coords)]
  (for [[x' y'] (coord-pairs [0 1 2])]
      [(+ x' x) (+ y' y)])
  )
)
(defn block-values [board coords]
  (set(map (fn [args] (value-at board args))
           (block-pairs coords)
    ))
)

(top-left [0 2])
(top-left [4 2])
(top-left [8 8])
(top-left [4 5])


(block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
(block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}