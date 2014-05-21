(defn tour? [startNode edges]
   (if (= (count edges) 0)
     true
     (let [availableEdges (filter (fn [edge] (some #{startNode} edge)) edges)]
       (some (fn [edge] (let [otherEdges (dissoc)])) availableEdges))))

(def tour? (fn [edges]
             (let [node (first(first edges))])


             ))

(= true (tour? [[1 2] [2 3] [3 4] [4 1]]))

(= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))