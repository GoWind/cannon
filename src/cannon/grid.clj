(ns cannon.grid
  (:require
    [cannon.objs :as o]
    [clojure.core.async :refer [>!! <!! chan thread alts!!]]))

(defn make-chans
  [p]
  (mapv (fn [x] (chan)) (range p)))

(defn transport
  [i chan-previous chan-next mat-a]
  (if (= 0 (mod i 2))
    (let [_ (>!! chan-previous mat-a)
          mat-new-a (<!! chan-next)]
      mat-new-a)
    (let [mat-new-a (<!! chan-next)
          _ (>!! chan-previous mat-a)]
      mat-new-a)))

(defn get-idx
  [i j p]
  (+ j (* i p)))


(defn get-chan-left
  [i j p chans]
  (nth chans (mod (- (+ j (* i p)) 1) p)))

(defn get-chan-right
  [i j p chans]
  (nth chans (mod (+ (+ j (* i p)) 1) p)))

(defn get-chan-up
  [i j p chans]
  (nth chans (mod (- p (get-idx i j p)) (* p p))))

(defn get-chan-down
  [i j p chans]
  (nth chans (mod (+ p (get-idx i j p)) (* p p))))

(defn compute
  [mat-a mat-b p i j chan-left chan-right chan-up chan-down]
  (let [c (o/serial-matrix-multiply mat-a mat-b)]
    (loop [k p c' c]
          (if (= k (- p 1))
            c'
            (let [new-a (transport i chan-left chan-right)
                  new-b (transport j chan-up chan-down)
                  new-c (o/serial-matrix-multiply new-a new-b)]
              (recur (inc k) new-c))))))

(defn collect-outputs
  "collect output matrix from all computing processes"
  [output-chans]
  (loop [m {} c output-chans]
        (if (empty? c)
          c
          (let [[[k output-matrix] chan] (alts!! [c])]
            (recur
              (assoc m k output-matrix)
              (into [] (remove (fn [k] (= k chan)) c)))))))

(defn launch-compute-threads
  "get the prepared submatrices, dimension of one side of grid
  and launch compute threads. return list of channels for output
  of each corresponding thread"
  [subs-a subs-b p]
  (let [row-chans (make-chans p)
        col-chans (make-chans p)
        k (int (Math/sqrt p))]
    (into []
          (for [row (range k)
                col (range k)
                :let [idx (+ col (* row k))]]
               (compute (get subs-a [row col])
                        (get subs-b [row col])
                        k
                        row
                        col
                        (get-chan-left idx p row-chans)
                        (get-chan-right idx p row-chans)
                        (get-chan-up row idx p col-chans)
                        (get-chan-down idx p col-chans))))))

(defn build-output-matrix [& args])

(defn multiply
  "prepare and start multiplying here"
  [mat-a mat-b]
  (let [[a-rows a-cols] (o/get-size mat-a)
        [b-rows b-cols] (o/get-size mat-b)
        grid-order (o/get-dimension-for-cannon a-rows a-cols b-rows b-cols)
        subs-a (o/prepare-matrix-a mat-a)
        subs-b (o/prepare-matrix-b mat-b)
        num-threads (* grid-order grid-order)
        out-chans (launch-compute-threads subs-a subs-b num-threads)
        outputs (collect-outputs out-chans)]
    (build-output-matrix outputs)))


