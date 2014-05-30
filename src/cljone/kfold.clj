(ns cljone.kfold
  (:require [clojure.java.io :as jio]
            [cljone.fileinput :as fi]
            [clojure.java.shell :as sh])
  (:gen-class :main true))

(defn -main [] (println "Nothing to see here"))

(defn remove-folds [filename number]
  (map #(do (sh/sh "rm" (str filename "-train" %))
            (sh/sh "rm" (str filename "-test" %)))
       (range number)))

(defn annotate-with-indices
  [coll]
  (map vector (iterate inc 0) coll))

(defn partition-text
  "Takes a file, and returns list of lists, each as long as a fold 
  (so (x y z) (u v w) when folds is 3 and each letter is a line.)"
  [filename folds]
  (with-open [rdr (jio/reader filename)]
    (doall (partition-all folds (line-seq rdr)))))

(defn write-folds
  "Writes to appropriate -testN for each partition, and each not N for -trainN
  So each train should match up with test, and you can do k-fold cross evaluation."
  [filename partitioned]
  (doseq [[ind value] (annotate-with-indices partitioned)]
  ;dirt simple writing out to files --- prob not webscale, prob not important
    (spit (str filename "-test" ind) 
          (str value "\n") :append true)
    (doall (map #(spit (str filename "-train" %) 
                (str value "\n") :append true) 
         (filter #(not= % ind) 
                 (range (count partitioned)))))))

(defn fold-file 
  [file folds]
  (map write-folds (repeat file) (partition-text file folds)))

(defn eval-folds
  [file folds]
  ;(fold-file file folds)
  (let [[testing training] 
        (map 
          #(map (fn [num] (str file "-" % num)) 
                (range folds)) 
          ["test" "train"])]
    (double ;going for mean of different trainings
      (/ (reduce + 
                 (pmap #(let [model (agent {})]
                          (fi/train-with-file %1 model)
                          (await model)
                          (fi/test-against-file %2 model)) 
                       testing training))
         folds))))

(defn kfold-eval
  [file folds]
  (do 
    (fold-file file folds)
    (let [kfeval (eval-folds file folds)]
      ;kfeval
      (remove-folds file folds)
      kfeval
      )
    )
  )

(kfold-eval "resources/trainingdata.txt" 10)

