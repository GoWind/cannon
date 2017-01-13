(ns cannon.objs-test
  (:require
    [clojure.test :refer :all]
    [cannon.objs :as objs]))


(def test-matrix [[1 2 3 4] [5 6 7 8] [9 10 11 12] [13 14 15 16]])

(deftest objs-tests
  (is (= [0 0 0] (objs/zeroes 3)))
  (is (= [0 0 0 0 0] (objs/zeroes 5))))

(deftest square-tests
  (is (= 4 (objs/next-square 3.5)))
  (is (= 9 (objs/next-square 7)))
  (is (= 1 (objs/next-square 0.4))))

(deftest dot-product-tests
  (is (= 32  (objs/dot-product [1 2 3] [4 5 6])))
  (is (= 3   (objs/dot-product [1 1 1] [1 1 1]))))

(deftest rpad-zeroes-tests
  (is (= [0 0 0 0] (objs/rpad-zeroes [] 4)))
  (is (= [1 2 3 0 0 0] (objs/rpad-zeroes [1 2 3] 6))))

(deftest col-pad-tests
  (is (= [[1 1 0 0] [1 2 0 0]] (objs/pad-cols-with-zeroes [[1 1] [1 2]] 4))))

(deftest transform-test
  (is (= [[1 4] [2 5] [3 6]] (objs/row-major->col-major [[1 2 3] [4 5 6]]))))

(deftest multiply-tests
  (is (= [[84 90 96] [201 216 231] [318 342 366]]
         (objs/serial-matrix-multiply
           [[1 2 3] [4 5 6] [7 8 9]]
           [[10 11 12] [13 14 15] [16 17 18]])))


  (is (=  [[22 28] [49 64]] (objs/serial-matrix-multiply [[1 2 3] [4 5 6]] [[1 2] [3 4] [5 6]]))))

(deftest subm-test
  (is (= [[11 12] [15 16]] (objs/get-submatrix test-matrix 1 1 2)))
  (is (= [[3 4] [7 8]] (objs/get-submatrix test-matrix 0 1 2))))

(deftest get-submatrices-test
  (let [p (objs/get-submatrices test-matrix 2)]
    (is (= {[0 0] [[1 2] [5 6]]
        [0 1] [[3 4] [7 8]]
        [1 0] [[11 12] [15 16]]
        [1 1] [[9 10] [13 14]]}
        (objs/align-left p 2)))
    (is (= {[0 0] [[1 2] [5 6]]
            [1 0] [[9 10] [13 14]]
            [0 1] [[11 12] [15 16]]
            [1 1]  [[3 4] [7 8]]} (objs/align-up p 2)))))


(deftest size-tests
  (is (= [3 3] (objs/get-size [[1 2 3] [4 5 6] [7 8 9]])))
  (is (= [2 3] (objs/get-size [[1 2 3] [4 5 6]]))))

(deftest prepare-tests
  (is (=   {[0 0] [[1 2] [4 5]]
            [0 1] [[3 0] [6 0]]
            [1 0] [[9 0] [0 0]]
            [1 1] [[7 8] [0 0]]}
            (objs/prepare-matrix-a [[1 2 3] [4 5 6] [7 8 9]] 2)))
  (is (= {[0 0] [[8 7] [2 1]]
           [0 1] [[6 0] [0 0]]
           [1 0] [[5 2] [0 0]]
           [1 1] [[3 0] [4 0]] }
        (objs/prepare-matrix-b [[8 7 3] [2 1 4] [5 2 6]] 2))))
(deftest multiply-tests
  (is (= true (objs/can-multiply? [[1 2 3] [4 5 6]] [[1 2] [3 4] [5 6]])))
  (is (= false (objs/can-multiply? [[1 2 3] [4 5 6]] [[1 2 3] [4 5 6]])))
  (is (= true (objs/can-multiply? [[1 2] [3 4]] [[1 2] [3 4]]))))

