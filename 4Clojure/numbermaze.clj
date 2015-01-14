(def step-functions
  [(fn [x] (* x 2))
   (fn [x] (when (= (rem x 2) 0) (quot x 2)))
   (fn [x] (+ x 2))])

(defn step-function [x]
  (remove nil?
          (map (fn [function]
                 (function x))
               step-functions)
          ))

(defn generate-new-level [current-level]
  (apply hash-set
         (mapcat step-function current-level)
         ))

(count
 (take-while (fn [elements] (not (elements 5)))
             (iterate generate-new-level #{1})))


(defn foo[x y]
  (inc
   (count
    (take-while (fn [elements] (not (elements y)))
                (iterate generate-new-level #{x})))
   ))


(def length-shortest
  (fn length-shortest[x y]
    (inc
     (count
      (take-while (fn [elements] (not (elements y)))
                  (iterate (fn  [current-level]
                             (set
                              (mapcat (fn [x]
                                        (remove nil?
                                                [(* x 2)
                                                 (when (= (rem x 2) 0) (quot x 2))
                                                 (+ x 2)])
                                        ) current-level))
                             ) #{x})))
     )))

(= 1 (length-shortest 1 1))

(length-shortest 1 1)
(length-shortest 3 12)
(length-shortest 12 3)
(length-shortest 5 9)
(length-shortest 9 2)
(length-shortest 9 12)
