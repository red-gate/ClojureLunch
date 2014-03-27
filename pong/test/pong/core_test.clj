(ns pong.core-test
  (:require [clojure.test :refer :all]
            [pong.core :refer :all]))

(deftest tests
  (testing "moving ball returns new ball with correct position"
    (is (= {:position [1 0]
            :velocity [1 0]} 
           (move-ball {:position [0 0]
                       :velocity [1 0]}
                      1))))
  (testing "moving objects returns new world with objects at new positions"
    (is (= [{:position [1 0]
             :velocity [1 0]
             :move move-ball
             :paint paint-ball}]
           (move-objs [{:position [0 0]
                        :velocity [1 0]
                        :move move-ball
                        :paint paint-ball}]
                      1))))
  (testing "can calculate bottom right corner correctly"
    (is (= [8 9] (bottom-right {:position [1 3] :size [8 7]}))))
  (testing "can calc bounding box"
    (is (= [1 3 8 9] (bounding-box {:position [1.1 3.4] :size [8 7]}))))
  (testing "solids are indexed correctly"
    (do (is (= {:idx 3 :obj {:solid? true}} (index-solid 3 {:solid? true})))
        (is (= nil (index-solid 3 {:solid? false})))))
  (testing "horizontal collisions are recognized correctly"
    (do (is (= :horz-coll (horz-coll? [0 0 4 4] [1 4 4 8])))
        (is (= nil (horz-coll? [0 0 4 4] [1 5 4 8])))
        (is (= nil (horz-coll? [0 0 4 4] [6 4 4 8])))))
  (testing "vertical collisions are recognized correctly"
    (do (is (= :vert-coll (vert-coll? [0 0 4 4] [4 4 4 8])))
        (is (= nil (vert-coll? [0 0 4 4] [5 4 4 8])))
        (is (= nil (vert-coll? [0 0 4 4] [4 5 4 8])))))
  (testing "collision between two objects is detected"
    (is (= [{:idx-objs [{:idx 0 
                         :obj {:position [0 0] :size [5 5]}}
                        {:idx 1 
                         :obj {:position [4 0] :size [5 5]}}]
            :type :vert-coll}]
           (detect-obj-colls {:idx 0 
                              :obj {:position [0 0] :size [5 5]}}
                             [{:idx 1
                               :obj {:position [4 0] :size [5 5]}}
                              {:idx 2
                               :obj {:position [5 0] :size [5 5]}}]))))
  (testing "collisions in world are detected"
    (is (= [{:idx-objs [{:idx 0 
                         :obj {:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                        {:idx 1 
                         :obj {:position [4 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}]
            :type :vert-coll}]
           (detect-collisions [{:position [0 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}
                               {:position [4 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}
                               {:position [5 0] 
                                      :size [5 5]
                                      :velocity [0 0]
                                      :solid? true}]
                              1))))
  (testing "obj's collide is called when objects collide, if it exists"
    (is (= ["called"
            {:position [4 0] 
             :size [5 5]
             :velocity [0 0]
             :solid? true}]
           (collide-obj {:idx 0 
                         :obj {:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                         {:idx 1 
                         :obj {:position [4 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}}
                        :vert-coll
                        [{:position [0 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true
                               :collide (fn [o oth ct] "called")}
                         {:position [4 0] 
                               :size [5 5]
                               :velocity [0 0]
                               :solid? true}]))))
  (testing "apply-collision calls collide-obj twice"
    (is (= 2 (with-redefs [collide-obj (fn [_ _ _ w] (inc w))] 
               (apply-collision 0 {}))))))
