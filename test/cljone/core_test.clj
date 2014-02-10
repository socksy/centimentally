(ns cljone.core-test
  (:require [clojure.test :refer :all]
            [cljone.core :refer :all]))

(defn dotraining [model] 
  ;a bit eww, perhaps refactor later
  (do (train! :positive "test" model)
      (train! :positive "test" model)
      (train! :negative "bad" model)
      (train! :negative "terrible" model)))

(defn setup-model [f]
  (def tmodel (atom {}))
  (def rmodel (atom {}))
  (dotraining rmodel)
  (f))

;(def tmodel (atom {}))
(use-fixtures :once setup-model)


(deftest train-test
  (testing "Can train basic"
    (is (= {:positive {"test" 1}}
           (do (train! :positive "test" tmodel)
               @tmodel)))
    (is (= {:positive {"test" 2}}
            (do (train! :positive "test" tmodel)
                @tmodel) ))
    (is (= {:negative {"bad" 1}, 
            :positive {"test" 2}}
           (do (train! :negative "bad" tmodel)
               @tmodel)))
    (is (= {:negative {"bad" 1 "terrible" 1}
            :positive {"test" 2} }
           (do (train! :negative "terrible" tmodel)
               @tmodel))))

  (testing "Category-total"
    (is (= 2
           (category-total :positive tmodel)))
    (is (= 2
           (category-total :negative tmodel)))))

(deftest feature-test
  (testing "Feature probabilities"
    (is (= 1
           (feature-probability :positive "test" rmodel)))
    (is (= 0
           (feature-probability :positive "nil" rmodel)))
    (is (= 1/2
           (feature-probability :negative "bad" rmodel)))
    (is (= 1/2
           (feature-probability :negative "terrible" rmodel))))
  
  (testing "Getting features"
    (is (= ["a" "test" "of" "feature" "extraction"]
           (get-features "a test of feature extraction"))))) 

(deftest text-test
  (testing "∝ probability of a text being in a category"
    (is (= 0
           (text-probability :positive "not in the category" rmodel)))
    (is (= 1 
           (text-probability :positive "test" rmodel)))
    (is (= 0.6931471805599453
           (text-probability :negative "terrible" rmodel)))
    (is (= 0.6931471805599453
           (text-probability :negative "bad" rmodel)))
    (is (= 1.3862943611198906 
           (text-probability :negative "terrible bad string" rmodel)))))

(deftest classify-test
  (testing "Classifications"
    (is (= :nil
           (classify "a string that is not in either dictionary" rmodel)))
    (is (= :positive
           (classify "a happy test string" rmodel)))
    (is (= :negative
           (classify "a terrible bad string" rmodel)))))

(run-tests)
