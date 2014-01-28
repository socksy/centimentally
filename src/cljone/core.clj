(ns cljone.core
  (:require [clojure.string :as string])
  (:import [java.lang Math])) 

(def model (atom {}))

(defn train!
  "Trains the model with the feature associating with the category symbol"
  [category feature]
  (swap! model update-in [category feature]
         (fnil inc 0)))

(defn category-total
  ^Number [category]
  (reduce + (vals (category @model))))

(defn feature-probability
  "Given a category and feature, probability the feature is in that category."
  [category feature]
  (let [feature-count (or (get (category @model) feature) 
                          0) ;not in hash-map means 0 probability
        total (category-total category)]
    (/ feature-count total)))

(defn get-features
  "Extract features from the text (atm just tokenizing)"
  [text]
   (string/split text #"\s+"))

(defn text-probability
  [category text]
  (reduce +
          (map #(let [prob (feature-probability category %)] 
                  (if (= 0 prob) 
                    0 
                    (- (java.lang.Math/log prob))))
               (get-features text))))

(defn classify [text] 
  (let [probs (into {}
        (map vector 
             (keys @model)
             (map #(text-probability % text)
                  (keys @model))))]
  (apply max-key #(% probs) (keys @model))))


(-> @model)
