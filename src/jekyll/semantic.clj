(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
  (:require [clojure.zip :as zip]))


(defn stringify [t]
  (apply str (flatten (map vals (flatten t)))))


(defn correct-bound [b]
  (if (= b \.)
    nil
    (let [[k v] (first b)] {k (stringify v)})))


(defn simplify-definitions [matched n]
  (if matched
    (if (contains? n :Expression)
      (let [mv (first (val (first n)))
            [k v] mv]
        (case k
          :String {:String (stringify v)}
          :Identity {:Identity (stringify v)}
          :Integer {:Integer (stringify v)}
          :Float {:Float (stringify v)}
          :Range {:Range [(correct-bound (first v)) (correct-bound (last v))]}
          :Boolean {:Boolean (let [[bk bv] (first v)] (= :True bk))}
          :Nil nil
          mv))
      {:Identity (stringify (val (first n)))})
      n))


(defn typify [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (map? %) (or
                             (contains? % :Identity)
                             (contains? % :Expression)))
             simplify-definitions))

(defn reduce-definition
  [matched n]
  (if matched
    (let [definition (val (first (first n)))]
      {:ID (first definition)
       :VALUE (second definition)
       :DEF (if (= (count definition) 2) '() (first (get (last definition) :Scope)))})
    n))

(defn reduce-tree [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (vector? %) (map? (first %)) (contains? (first %) :Definition))
             reduce-definition))

(-> "l = [1 2 3] b = 5"
            parse
            clean-parse-tree
            typify
            reduce-tree
            )
