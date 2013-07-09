;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

;; expr ::= term <+> expr | term
;; term ::= atom <*> term | atom
;; atom ::= 0 | 1 | 2 | ...

(def numeric?
    #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(declare parse-atom
         parse-op)

(defn parse-expr*
    [input position]
  (let [term (parse-term input position)]
    (when term
      (let [op (parse-op input (second term) \+)]
        (if-not op
          term
          (let [expr (parse-expr input (second op))]
            (if-not expr
              expr
              [[(first op) (first term) (first expr)] (second expr)])))))))

(def parse-expr (memoize parse-expr*))

(defn parse-op*
      [input position op]
  (when (< position (count input))
    (let [input (subvec input position)
          token (first input)]
      (when (= op token)
        [(keyword (str op)) (inc  position)]))))

(def parse-op (memoize parse-op*))

(defn parse-term*
  [input position]
  (let [atom1 (parse-atom input position)]
    (when atom1
      (let [op (parse-op input (second atom1) \*)]
        (if-not op
          atom1
          (let [term (parse-term input (second op))]
            (if-not term
              atom1
              [[(first op) (first atom1) (first term)] (second term)])))))))

(def parse-term (memoize parse-term*))

(defn parse-atom*  [input position]
  (when (< position (count input))
    (let [input (subvec input position)
          token (take-while numeric? input)]
      (when (seq token)
        (let [num (Integer/parseInt (apply str token))
              pos (+ position (count token))]
          [num pos])))))

(def parse-atom (memoize parse-atom*))

(parse-expr (vec "1+2*1+2+1+1+1+1+1+1") 0)

(doc keyword)