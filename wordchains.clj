; A word chain consists of a set of words ordered so that
; each word differs by only one letter from the words directly
; before and after it. The one letter difference can be either
; an insertion, a deletion, or a substitution.
; Here is an example word chain:
;
; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;
; Ex: (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(defn ziplist [l1 l2]
  (map vector l1 l2))

(defn substitution? [w1 w2]
  (= 1
     (count (filter (fn [[a b]] (not= a b))
                    (ziplist w1 w2)))))

(defn asdf? [[h1 & t1] [h2 & t2]]
  (if (or (nil? h1) (nil? h2))
    true
    (if (= h1 h2)
      (recur t1 t2)
      (= t1 (cons h2 t2)))))

(defn addition? [w1 w2]
  (when (= 1 (- (count w1) (count w2)))
    (asdf? w1 w2)))

(defn link? [w1 w2]
  (or (addition? w1 w2)
      (addition? w2 w1)
      (substitution? w1 w2)))

(defn word-chain?
  ([words]
   (->> words
        (map #(word-chain? % (remove #{%} words)))
        (some #{true})
        (= true)))
  ([start-word words]
   (if-not (seq words)
     true
     (some #(word-chain? % (remove #{%} words))
        (filter #(link? start-word %) words)))))

(= true (word-chain? #{"hat" "dog" "coat" "cat" "hog" "oat" "cot" "hot"}))
(= true (word-chain? #{"hat" "dog" "coat" "hog" "oat" "cot" "hot"}))
(= false (word-chain? #{"cot" "hot" "bat" "fat"}))
(= false (word-chain? #{"to" "top" "stop" "tops" "toss"}))
(= true (word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (word-chain? #{"share" "hares" "shares" "hare" "are"}))
(= false (word-chain? #{"share" "hares" "hare" "are"}))

(substitution? "cat" "cot")
(substitution? "cat" "act")
(substitution? "cat" "coat")

(addition? "coat" "cat")
(= false (addition? "coat" "kit"))

(asdf? "coat" "cat")
(asdf? "coat" "cut")
(asdf? "cat" "cat")

(link? "coat" "cat")
(link? "cat" "coat")
(link? "cat" "cit")
(link? "cat" "act")
(link? "cat" "cat")
