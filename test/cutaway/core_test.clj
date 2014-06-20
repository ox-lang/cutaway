(ns cutaway.core-test
  (:refer-clojure :exclude [resolve])
  (:require [cutaway.core :refer :all]
            [clojure.test :refer :all]))

(deftest basic
  (let [cutaway       (make-cutaway {})
        [cutaway id1] (decompose cutaway {:foo :bar})
        [cutaway id2] (decompose cutaway {:quxx id1})]
    (is (= (get cutaway id1) {:foo :bar}))
    (is (= (get cutaway id2) {:quxx id1}))
    (is (= (resolve cutaway (fetch cutaway id1))
           {:foo :bar}))
    (is (= (resolve cutaway (fetch cutaway id2))
           {:quxx {:foo :bar}}))


    (let [v (resolve cutaway (fetch cutaway id1))
          [cutaway id] (decompose cutaway (assoc v :foo :baz))]
      (is (= (get cutaway id1) {:foo :baz}))
      (is (= (resolve cutaway (fetch cutaway id2))
             {:quxx {:foo :baz}})))))
