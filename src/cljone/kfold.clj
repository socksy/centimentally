(ns cljone.kfold
  (:require [clojure.java.io :as jio]
            [cljone.fileinput :as fi]
            [clojure.java.shell :as sh])
  (:gen-class :main true))

(defn -main [] (println "Nothing to see here"))


(fold-file "resources/trainingdata.txt" 6)
(remove-folds "resources/trainingdata.txt" 6)

(defn remove-folds [filename number]
  (map #(do (sh/sh "rm" (str filename "-train" %))
            (sh/sh "rm" (str filename "-test" %)))
       (range number)))

(defn fold-file 
  [file folds]
  (map write-folds (repeat file) (partition-text file folds)))

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
  [filename partitioned]
  (doseq [[ind value] (annotate-with-indices partitioned)]
    (spit (str filename "-test" ind) 
          (str value "\n") :append true)
    (doall (map #(spit (str filename "-train" %) 
                (str value "\n") :append true) 
         (filter #(not= % ind) 
                 (range (count partitioned)))))))
