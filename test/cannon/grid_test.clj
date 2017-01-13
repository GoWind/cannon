(ns cannon.grid-test
  (:require
    [clojure.test :refer :all]
    [cannon.grid :as g]))

(def vec-of-ints (into [] (range 64)))
(def side-len 8)

