
;; We now extend this to a very simple functional language

;; equality ::= expr = expr 

;; expression ::= bracketedexpr whitespace application | bracketedexpr
;; bracketedexpr ::= ( expr ) | expr
;; expr ::= term <+> expr | term <-> expr | term
;; term ::= atom <*> term | atom
;; identifierOrAtom ::= atom | identifier
;; atom ::= 0 | 1 | 2 | ...
;; identifier ::= (a-zA-Z)*

(def numeric?
    #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def identifier? (set (map char (concat (range 65 91) (range 97 123)))))

(def whitespace? (set '(\newline \space)))

(declare parse-atom parse-op parse-term parse-equality parse-identifier-or-atom
         parse-bracketed-expression parse-expression parse-whitespace)

(defn parse-equality*
    [input position]
  (let [term (parse-expr input position)]
    (when term
      (let [op (parse-op input (second term) \=)]
        (if-not op
          term
          (let [expr (parse-expr input (second op))]
            (if-not expr
              expr
              [[(first op) (first term) (first expr)] (second expr)])))))))

(def parse-equality (memoize parse-equality*))

(defn parse-expr*
    [input position]
  (let [term (parse-term input position)]
    (when term
      (let [op (or (parse-op input (second term) \+)
                   (parse-op input (second term) \-))]
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
  (let [atom1 (parse-identifier-or-atom input position)]
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

(defn parse-identifier*  [input position]
  (when (< position (count input))
    (let [input (subvec input position)
          token (take-while identifier? input)]
      (when (seq token)
          [(apply str token) (+ position (count token))]))))

(def parse-identifier (memoize parse-identifier*))

(defn parse-whitespace*  [input position]
  (when (< position (count input))
    (let [input (subvec input position)
          token (take-while whitespace? input)]
      (when (seq token)
          [(apply str token) (+ position (count token))]))))

(def parse-whitespace (memoize parse-whitespace*))

(defn parse-identifier-or-atom*
    [input position]
  (or (parse-atom input position) 
      (parse-identifier input position)))

(def parse-identifier-or-atom (memoize parse-identifier-or-atom*))

(defn parse-bracketed-expression*
  [input position]
   (when (< position (count input))
     (or 
      (when (= (input position) \()
        (let [expr (parse-expr input (+ position 1))]
          (when expr
            (let [end (second expr)]
               (when (< end (count input))
                 (when (= (input end) \))
                   [(first expr) (+ end 1)]))))))     
      (parse-expr input position))))

(def parse-bracketed-expression (memoize parse-bracketed-expression*))

(defn parse-expression*
  [input position]
  (let [expr1(parse-bracketed-expression input position)]
    (when expr1
      (or (let [whitespace (parse-whitespace input (second expr1))]
            (when whitespace
               (let [application (parse-expression input (second whitespace))]
                 (when application
                   [[(first expr1) (first application)] (second application)]))))
          expr1))))
  
(def parse-expression (memoize parse-expression*))

(parse-expression (vec "a (1+2)") 0)



