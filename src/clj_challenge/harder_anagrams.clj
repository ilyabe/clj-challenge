(ns clj-challenge.harder-anagrams
  (:require [clojure.set :as clj-set]
            [clojure.string :as cs]))

;;
;; Building blocks
;;

(def dictionary
  (-> (slurp "https://gist.githubusercontent.com/ericnormand/8c0ccc095edaa64eb8e00f861f70b02c/raw/01c33b3438bbab6bdd7e8dade55c1f5997ad8027/wordlist.txt")
      (cs/split-lines)
      (set)))

(def alphabet ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"])

(defn prime?
  "The goal of this problem isn't to find how to calculate prime numbers, so let's
  use Java's implementation."
  [x]
  (let [certainty 5] (.isProbablePrime (BigInteger/valueOf x) certainty)))

(def primes
  "Lazy sequence of prime numbers"
  (filter prime? (range)))

(def alphabet-primes
  "A map of each letter of the English alphabet mapped to a unique prime number"
  (into {} (map #(vector %1 %2) alphabet primes)))

;;
;; Utils
;;

(defn clean
  "Lower cases and removes all the spaces from a phrase"
  [phrase]
  (-> phrase
      (str)
      (cs/lower-case)
      (cs/replace #" |\'" "")))

(defn phrase->id
  "Converts a phrase to the product of the prime number values of each letter."
  [phrase]
  (->> (clean phrase)
       (map #(get alphabet-primes (cs/lower-case (str %))))
       (reduce * 1.0)))

;;
;; Let's find some anagrams
;;

(defn anagrams?
  "Checks if two phrases are anagrams"
  [phrase-one phrase-two]
  (let [p-one (clean phrase-one)
        p-two (clean phrase-two)]
    (= (phrase->id p-one) (phrase->id p-two))))

(defn make-anagrams
  "Takes a phrase and returns a vector of sets of anagrams."
  [phrase]
  (let [phrase-id (phrase->id phrase)]
    (loop [dict dictionary
           result []]
      (if (empty? dict)
        result
        (let [maybe-anagrams (reduce (fn [m candidate]
                                       (if (= 0.0 (mod (:phrase-id m) (phrase->id candidate)))
                                         (-> m
                                             (update :anagrams (fn [coll] (conj coll candidate)))
                                             (assoc :phrase-id (/ (:phrase-id m) (phrase->id candidate))))
                                         m))
                                     {:phrase-id phrase-id
                                      :anagrams #{}}
                                     dict)]
          (cond
            ;; We found a set of anagrams
            (= 1.0 (:phrase-id maybe-anagrams)) (recur (clj-set/difference dict (:anagrams maybe-anagrams))
                                                       (conj result (:anagrams maybe-anagrams)))
            ;; No anagrams found, we're done
            (= (:phrase-id maybe-anagrams) phrase-id) (recur [] result)
            ;; Found some candidate anagrams, but not enough, this is where things
            ;; don't go quite right and we remove some candidates that could group
            ;; with others to form an anagram
            (> (:phrase-id maybe-anagrams) 1.0) (recur (clj-set/difference dict (:anagrams maybe-anagrams))
                                                       result)))))))

;;
;; A few REPL expressions to try things out
;;
(comment
  (require 'clj-challenge.harder-anagrams :reload)
  (anagrams? "School master" "The classroom")
  (anagrams? "Astronomer" "Moon starer")
  (anagrams? "The eyes" "They see")
  (anagrams? "These are" "not anagrams")
  (make-anagrams "kcor")
  (make-anagrams "rock party")
  (prime? 13)
  (take 26 primes)
  alphabet-primes
  (phrase->id "rock")
  (phrase->id "somebody move")
  dictionary)
