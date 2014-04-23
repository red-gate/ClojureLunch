
(def foo "aaaaa")

foo

(.toString foo)


(defrecord my-coll [els]
  Object
    (toString [this] (apply str (interpose ", " (sort els))))
  clojure.lang.ISeq
    (next [this] (my-coll. (rest els)))
    (first [this] (first els))
    (more [this] (my-coll. (rest els)))
  )

(.toString a)

(first a)

(second a)

(
(fn [& args]
(defrecord my-coll [els]
  Object
    (toString [this] (apply str (interpose ", " (sort els))))
  clojure.lang.ISeq
    (next [this] (my-coll. (rest els)))
    (first [this] (first els))
    (more [this] (my-coll. (rest els)))
	)
  (my-coll. args)
)
1 2 3)

(do (def a (my-coll. [1 2 3])) nil)

