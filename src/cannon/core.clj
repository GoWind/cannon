(ns cannon.core
  (:require
    [cannon.io :as i]
    [cannon.grid :as g]
    [cannon.objs :as o])
  (:gen-class))

(declare start-computing)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (if (not (= (count args) 3))
    (IllegalArgumentException. "params must be matrix-a file matrix-b file output-file names")
    (apply start-computing args)))

(comment
(defn start-computing
  [mat-a-f mat-b-f out-f]
  (let [mat-a (i/read-matrix-file mat-a-f)
        mat-b (i/read-matrix-file mat-b-f)]
    (if (not (o/can-multiply? mat-a mat-b))
      (do
        (println "cannot multiply matrices of size "
                 (o/get-size mat-a)
                 " "
                 (o/get-size mat-b))
        (System/exit 1))
      (println (g/multiply mat-a mat-b))))))


