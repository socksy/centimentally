(ns cljone.core
  (:require [clojure.string :as string])
  (:import [java.lang Math]))

(defn get-features
  "Extract features from the text (atm just tokenizing)"
  [text]
    (string/split (.toLowerCase text) #"\W"))

(defn train
  "Trains the model with the feature associating with the category symbol"
  [category feature model]
  (send model update-in [category feature]
         (fnil inc 0)))

(defn train-text
  "Trains the model with a category and text using train"
  [category text model]
  ;just removed a doall from around this, figure it's no use
  ;as there's a do-all in the train-over-words function
  (map train (repeat category) (get-features text) (repeat model)))

(defn category-total
  ^Number [category model]
  (reduce + (vals (category @model))))

(defn feature-probability
  "Given a category and feature, probability the feature is in that category."
  [category feature model]
  (let [feature-count (or (get (category @model) feature) 
                          0) ;not in hash-map means 0 probability
        total (category-total category model)]
    (/ feature-count total)))

;because of the way this naive bayes works, this isn't strictly a probability of
;as in the case of a percent or out of 1
(defn text-probability
  [category text model]
  (reduce +
          (map #(let [prob (feature-probability category % model)] 
                  (if (or (= 0 prob) (= 1 prob)) 
                    prob 
                    (- (java.lang.Math/log prob))))
               (get-features text))))

(defn classify [text model] 
  "Classifies a given text with a given model. Returns :nil on unclassified."
  (let [probs (into {}
        (map vector 
             (keys @model)
             (map #(text-probability % text model)
                  (keys @model))))]
  (if-not (apply distinct? (vals probs))
    :nil
    (apply max-key #(% probs) (keys @model)))))


;(-> @model)
