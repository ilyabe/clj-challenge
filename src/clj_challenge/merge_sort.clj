(ns clj-challenge.merge-sort
  "Sorts numbers using the Merge Sort algorithm"
  (:require [clojure.set :as clj-set]
            [clojure.string :as cs]))

(defn do-merge
  "Merges two sorted sequences - `left` and `right`. The sequences are
  destructured as e.g.
    x - the first element
    xs - the rest of the elements
    lt - the whole left sequence
   If either first element is `nil` then we can safely append them to the result.
   Otherwise, we still have elements, and so we append the smaller of the two onto
   the result of a recursive call with the rest of the elements from that sequence
   and the other sequence."
  [left right]
  (loop [[x & xs :as lt] left
         [y & ys :as rt] right
         result []]
    (cond
      (or (nil? x) (nil? y)) (concat result lt rt)
      (< x y)                (recur xs rt (conj result x))
      :else                  (recur lt ys (conj result y)))))

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
  (do-merge [1] [2 5])
  ;; Make sure there's no StackOverflow
  (do-merge (range 1000000) (range 1000000 2000000))
  (merge-sort (take 1000000 (map rand-int (repeat 1000000)))))
