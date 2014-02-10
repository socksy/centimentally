(ns cljone.fileinput
  (:require [clojure.string :as string]
            [clojure.java.io :as jio]
            [cljone.core :refer [train-text]])
  (:import []))

;want to be able to support bigger files than memory can support, so using
;buffered reader rather than slurp.
(defn read-and-do [filename func ] 
  (with-open [rdr (jio/reader filename)]
    (func (line-seq rdr))))

(line-count "resources/trainingdata.txt")
(with-open [rdr (jio/reader "resources/trainingdata.txt")] (count (line-seq rdr)))

(defn line-count [filename]
  (read-and-do filename count))

(defn split-up [line]
  (let [[_ classification tweet] (re-matches #"\"(.+)\",\"(.+)\"" line)]
    [(keyword classification) tweet]))

(defn train-with-file 
  "Given filename, model and optional number of lines, trains the model"
  ([filename model]
   (train-with-file filename (line-count filename) model))
  ([filename no-of-lines model] 
   (read-and-do filename
     (partial train-over-words model no-of-lines))))

(defn train-over-words [model no-of-lines lines-read] 
   (doall (map (fn [values] (let [[cat text] values]
                              (if-not (or (nil? cat) (nil? text))
                                (train-text cat text model)))) 
               (map #(split-up %) (take no-of-lines lines-read))))) 
