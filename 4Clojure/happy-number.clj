(defn digits [x]
  (if (= x 0)
      nil
    (conj
     (digits
      (quot x 10))
     (rem x 10))))

(defn step-number[x]
  (apply +
   (map
    (fn [i] (* i i))
    (digits x))))


(defn happy-set [x s]
  (if (s x)
      false
    (let [y (step-number x)]
	 (if (= 1 x)
	     true
	   (happy-set y (conj s x))))))

(defn happy-number [x]
  (happy-set x #{}))

(digits 52345)
(step-number 7)

(step-number 1)

(happy-number 7)
(happy-number 3)
(happy-number 986543210)
(happy-number 2)
(happy-number 10000)
(happy-number 1)

(def happy-number
(letfn [( digits [x]
  (if (= x 0)
      nil
    (conj
     (digits
      (quot x 10))
     (rem x 10))))

( step-number[x]
  (apply +
   (map
    (fn [i] (* i i))
    (digits x))))


(happy-set [x s]
  (if (s x)
      false
    (let [y (step-number x)]
	 (if (= 1 x)
	     true
	   (happy-set y (conj s x))))))

( happy-number [x]
  (happy-set x #{}))]


  happy-number)
)

(happy-number 7)
(happy-number 3)
(happy-number 986543210)
(happy-number 2)
(happy-number 10000)
(happy-number 1)




