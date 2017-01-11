(ns cannon.io
  (:require [clojure.string :refer :all]))

(defn read-matrix-file
  "read a csv file that stores matrix in row major format"
  [filename]
  (let [p (slurp filename)]
    (->>
      (split p #"\n")
      (mapv (fn [row] (->> (split row #",") (mapv #(Integer/parseInt %))))))))

