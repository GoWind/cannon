(ns cannon.objs)

(defn can-multiply
  [mat-a mat-b]
  (let [a-cols (count (nth mat-a 0))
        b-rows (count mat-b)]
    (= a-cols b-rows)))

(defn next-square
  "return the integer that is an
   integer square"
  [n]
  (int (Math/pow (Math/ceil (Math/sqrt n)) 2)))

(defn get-size
  [mat]
  (let [rows (count mat)
        cols (count (nth mat 0))]
    [rows cols]))

(defn can-multiply?
  [mat-a mat-b]
  (let [b-rows (count mat-b)
        a-cols (count (nth mat-a 0))]
    (= a-cols b-rows)))

(defn get-dimension-for-cannon
  "return the dimension of each side of matrix that is an integer square"
  [a-rows a-cols b-rows b-cols]
  (next-square (max a-rows a-cols b-rows b-cols)))


(defn int-square?
  [n]
  (let [s (Math/sqrt n)]
    (= (Math/ceil s) (Math/floor s))))

(defn zeroes
  [n]
  (into [] (take n (repeat 0))))

(defn rows-of-zeroes
  "return n rows of r zeroes"
  [n r]
  (mapv (fn [x] (zeroes r)) (range n)))


(defn dot-product
  "dot product of a and b"
  [a b]
  (reduce + (map * a b)))

(defn rpad-zeroes
  "right pad row with zeroes for the given length"
  [row dim]
  (let [len (count row)]
    (if (> len dim)
      row
      (into [] (concat row (take (- dim (count row)) (repeat 0)))))))

(defn pad-cols-with-zeroes
  "extend vector v with zeroes on the right
   so that v's count is dim"
  [v dim]
  (mapv (fn [x] (rpad-zeroes x dim)) v))

(defn pad-matrix
  "pad a matrix m so that m is pad-sizexpad-size"
  [m pad-size]
  (let [rows (count m)]
    (if (> pad-size rows)
      (vec (concat (pad-cols-with-zeroes m pad-size) (rows-of-zeroes (- pad-size rows) pad-size)))
      (pad-cols-with-zeroes m pad-size))))


(defn row-major->col-major
  "convert a matrix m from row major layout
   to col major layout"
  [m]
  (let [cols (count (nth m 0))]
    (into []
        (for [i (range cols)]
             (mapv (fn [x] (nth x i)) m)))))


(defn serial-matrix-multiply
  "do a normal matrix multiplication"
  [ma mb]
  (let [mbc (row-major->col-major mb)
        cols (count (nth mb 0))]
   (partition
     cols
     (->>
       (for [r ma
             c mbc]
            (dot-product r c))))))


(defn get-submatrix
  [m i j p]
  (let [row-start (* i p)
        col-start (* j p)]
    (mapv (fn [row] (subvec row col-start (+ col-start p))) ;; fetch cols in the row in range (j*p) (j*p) + p
          (subvec m row-start (+ row-start p))))) ;; fetch rows in range i*p -> (i*p)+p

(defn get-submatrices
  "given a matrix and order of the submatrix,split matrix into
   pxp blocks and return the submatrices"
  [m p]
  (into {} ;; need this to prevent laziness
        (for [i (range p)
              j (range p)]
          [[i j] (get-submatrix m i j p)])))

(defn align-left
  "m has been split into subXsub matrices
   adjust rows such that each sub-matrix in a row
   is shifted one position left, wrapping over"
  [m sub]
  (loop [m' m new-m {}]
        (if (empty? m')
          new-m
          (let [[[row col] matrix] (first m')]
            (recur
              (rest m')
              (assoc new-m [row (mod (- col row) sub)] matrix))))))

(defn align-up
  [m sub]
  (loop [m' m new-m {}]
        (if (empty? m')
          new-m
          (let [[[row col] matrix] (first m')]
            (recur
              (rest m')
              (assoc new-m [(mod (- row col) sub) col] matrix))))))

(defn prepare-matrix-a
  "split matrix m into submatrices and align left m"
  [m p]
  (let [padded-matrix (pad-matrix m (* p p))
        submatrices (get-submatrices padded-matrix p)
        aligned-matrix (align-left submatrices p)]
    aligned-matrix))

(defn prepare-matrix-b
  "split matrix m into submatrices and align up m"
  [m p]
  (let [padded-matrix (pad-matrix m (* p p))
        submatrices (get-submatrices padded-matrix p)
        aligned-matrix (align-up submatrices p)]
    aligned-matrix))
