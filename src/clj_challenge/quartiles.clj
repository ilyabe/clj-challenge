(ns clj-challenge.quartiles)

(defn median
  "Returns the median value of a collection. The count `cnt` can be passed in to
  save time."
  ([coll]
   (median coll (count coll)))
  ([coll cnt]
   (cond
     (odd? cnt) (nth coll (/ cnt 2))
     (even? cnt) (/ (+ (nth coll (/ cnt 2))
                       (nth coll (/ (- cnt 1) 2)))
                    2))))

(defn halves
  "Returns the ((left) (right)) halves of a collection. The count `cnt` can be passed
  in to save time."
  ([coll]
   (halves coll (count coll)))
  ([coll cnt]
   (cond
     (odd? cnt) (let [[left right] (split-at (/ cnt 2) coll)]
                  (list (butlast left) right))
     (even? cnt) (split-at (/ cnt 2) coll))))

(defn quartiles
  "Returns the quartiles of a collection, like this:
    {:q0 1  ;; the min
     :q1 2  ;; 25th percentile
     :q2 3  ;; the median
     :q3 4  ;; 75th percentile
     :q4 5} ;; the max"
  [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        cnt-half (quot cnt 2)
        [left right] (halves sorted cnt)]
    {:q0 (first sorted)
     :q1 (median left cnt-half)
     :q2 (median sorted cnt)
     :q3 (median right cnt-half)
     :q4 (last sorted)}))

;; REPL expressions to try things out
(comment
  ;; Sample data
  (def even [1 10 2 9 3 8 4 7 5 6])
  (def odd [1 10 2 9 3 8 4 7 5 6 11])
  (def big (take 1000000 (map rand-int (repeat 1000000))))
  ;; Get median, halves
  (median even)
  (median odd)
  (halves even)
  (halves odd)
  ;; Even number of elements
  (quartiles even)
  ;; Odd number of elements
  (quartiles odd)
  ;; Big random numbers
  (time (quartiles big)))
