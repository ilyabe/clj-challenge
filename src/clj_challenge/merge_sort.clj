(ns clj-challenge.merge-sort
  "Sorts numbers using the Merge Sort algorithm"
  (:require [clojure.set :as clj-set]
            [clojure.string :as cs]))

(defn do-merge
  "Takes a left and right sequence, destructured to e.g.
    x - the first element
    xs - the rest of the elements
    left - the whole sequence
   If the first element in either sequence is nil, i.e. it's an empty sequence,
   then we know we can simply combine the two. Otherwise, we put the smaller of
   the first elements onto the front of the result of do-merge of the others."
  [[x & xs :as left] [y & ys :as right]]
  (cond
    (or (nil? x) (nil? y)) (concat left right)
    (< x y)                (cons x (do-merge xs right))
    :else                  (cons y (do-merge left ys))))

(defn merge-sort [arg]
  (let [cnt (count arg)
        mid (/ cnt 2)
        left (take mid arg)
        right (drop mid arg)]
    (if (= 1 cnt)
      arg
      (do-merge (merge-sort left) (merge-sort right)))))

;;
;; A few REPL expressions to try things out
;;
(comment
  (def data1 [1 9 2 8 3 7 4 6 5])
  (def data2 [1 9 2 8 3 7 4 6 5 10])
  (merge-sort data1)
  (merge-sort data2)
  (do-merge [1 4 4 5] [2 3 7 9])
  (do-merge [1] [2 5]))
