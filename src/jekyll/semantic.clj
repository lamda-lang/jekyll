(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
  (:require [clojure.zip :as zip]))


(defn stringify-token [t]
  (do (println (filter list? t))
      (apply str (flatten t))))


(defn java-value [matched n]
  (if matched
    (if (contains? n :Expression)
      (let [[k v] (first (:Expression n))]
        (case k
          :String (stringify-token (map vals (second v)))
                                        ;        :Nummeric (. Integer parseInt (stringify-token (map vals v))) ; only works for single digit atm.
                                        ;        :Numeric (. Integer parseInt (reduce str (map #(val (first %)) (flatten v))))
          :Boolean (let [[bk bv] (first v)] (= :True bk))
          :Nil nil
          n))
      (reduce str (map #(val (first %)) (flatten (val (first n)))))
      )
    n))


(defn typify [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (map? %) (or (contains? % :Identity) (contains? % :Expression)))
             java-value))


(defn reduce-tree
  [typified-parse-tree]
  (vec (map #(:Definition (first %)) (first (:Module typified-parse-tree)))))


(comment
  (-> "s = true \n h = \"Hello sailor!\" \n numBer=333 \n float = 234"
              parse
              clean-parse-tree
              typify
              ))



(-> "abc = \"xyz\" foo = \"bar\" z1 = a where a = 2 end "
    parse
    clean-parse-tree
    typify
    reduce-tree)
