
(def foo "aaaaa")

foo

(.toString foo)


(defrecord my-coll [els]
  Object
    (toString [this] (apply str (interpose ", " (sort els))))
  clojure.lang.ISeq
  (first [this] (first els))
  (next [this] (my-coll. (rest els)))
  (more [this] (my-coll. (rest els)))
  )

(defmethod print-method my-coll [v w] (.write w "ajaja"))

(.toString a)

(first a)

(second a)

(do (def a (my-coll. [1 2 3])) nil)

(defn make-one [& els]
  (reify
  Object
  (toString [this] (apply str (interpose ", " (sort els))))
  clojure.lang.ISeq
  (first [this] (first els))
  (more [this] (make-one (rest els)))
  (next [this] (when (> (count els) 1)
                 make-one (rest els)))
  clojure.lang.Seqable
  (seq [this] (when (> (count els) 0)
                (apply make-one (reduce #(if (some #{%2} %1) %1 (conj %1 %2)) [] els))))
    clojure.lang.Sequential
  ))

(let [a (make-one '(1 2 3 4))]
  (. a (toString)))

(= '(2 1 3) (make-one 2 1 3))

(make-one 2 1 3)

(make-one 2 1 3 3 1 2)

(make-one)

(seq (make-one))
(str (make-one))


