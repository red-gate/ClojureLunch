(def board1 [[:e :e :e]
            [:e :e :e]
            [:e :e :e]])

(def board2 [[:x :x :x]
           [:x :o :e]
           [:o :e :x]])

(def board3 [[:x :o :x]
           [:x :o :e]
           [:o :o :e]])

(def board4 [[:x :o :x]
           [:x :x :e]
           [:o :e :x]])

(def board5 [[:o :o :x]
           [:x :x :e]
           [:x :e :x]])

(defn transpose [board]
  (apply map (fn[a b c]  [a b c] ) board))

(defn get-leading-diagonal [board]
  (for [i (range 0 3)]
    (get-in board [i i])
  ))

(defn get-diagonals [board]
  [(get-leading-diagonal board) (get-leading-diagonal (vec (map (fn [x] (vec (reverse x))) board)))])

(defn get-winner [board]
  (let
    [extended-board (concat board (transpose board) (get-diagonals board))]
  (ffirst
   (filter  (fn [row]
              (and (= (count (set row)) 1)
                   (not= (first row) :e)))
            extended-board))))


(transpose board2)

(get-winner board1)

(get-winner board2)

(get-winner board3)

(get-winner board4)

(get-winner board5)

