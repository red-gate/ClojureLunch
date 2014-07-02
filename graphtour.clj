

(def tour?
  (letfn [(contains-el?  [coll el]
               (some #{el} coll))
          (tour-from-node? [startNode edges]
   (if (= (count edges) 0)
     true
     (let [availableEdges (filter
                           (fn [edge]
                             (contains-el? edge startNode))
                           edges)]
       (some
        (fn [edge]
          (let [[before after] (split-with #(not= edge %) edges)
                remaining-edges (concat before (rest after))]
            (tour-from-node? (if (= (first edge) startNode) (second edge) (first edge)) remaining-edges)
            ))
        availableEdges))))]
      (fn [edges]
             (let [node (first(first edges))]
    ( not= nil (tour-from-node? node edges))))

             ))

(tour? [[1 2]])

(tour? [[1 2] [2 1]])

(tour? [[1 2] [2 3] [3 1]])


(= true (tour? [[1 2] [2 3] [3 4] [4 1]]))


(tour? [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]])

(some (fn [x] true) [])




(= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))

(split-with #(not= [2 3] %) [[1 2][2 3][4 5]])
