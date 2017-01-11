(ns cannon.io-test
  (:require [cannon.io :as i]
            [clojure.test :refer :all]))

(def matrix "1,2,3,4,5\n6,7,8,9,10\n")
(def m2 "1,2,3,4,5\n6,7,8,9,10")
(deftest matrix-read-test
  (is (= [[1 2 3 4 5] [6 7 8 9 10]] (i/read-matrix-file
                                      (java.io.StringReader. matrix))))
  (is (= [[1 2 3 4 5] [6 7 8 9 10]] (i/read-matrix-file
                                      (java.io.StringReader. m2)))))
