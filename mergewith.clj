(defn merge-with [f initial-map & maps]
  (reduce (fn [result map]
            (reduce (fn [result [k v]]
                      (if(contains? result k)
                        (assoc result k (f v (result k)))
                        (assoc result k v)))
                        result map))
          initial-map maps))


(= (merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})

(= (merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})