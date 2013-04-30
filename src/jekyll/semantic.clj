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


(defn typify-definition [matched n]
  (if matched
    (if (contains? n :Expression)
      (let [mv (first (val (first n)))
            [k v] mv]
        (case k
          :String (stringify v)
          :Identity (symbol (stringify v))
          :Identifier (keyword (stringify (val (first (first v)))))
          :Integer  (. Integer parseInt (stringify v))
          :Float (. Float parseFloat (stringify v))
          :Range {:Range [(correct-bound (first v)) (correct-bound (last v))]}
          :Boolean (let [[bk bv] (first v)] (= :True bk))
          :Nil nil
          mv))
      (symbol (stringify (val (first n)))))
      n))


(defn typify [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (map? %) (or
                             (contains? % :Identity)
                             (contains? % :Expression)))
             typify-definition))


(defn simplify-definition [matched n]
  (if matched
    (let [definition (val (first (first n)))]
      {(first definition) (second definition)
       :DEF (if (= (count definition) 2) '() (first (get (last definition) :Scope)))})
    n))


(defn reduce-definitions [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (vector? %) (map? (first %)) (contains? (first %) :Definition))
             simplify-definition))


(defn simplify-collection [matched n]
  (if matched
    (case (first n)
      :Set (set (map first (first (second n))))
      :Map (apply array-map (flatten (map
                                      #(vec [(keyword (first (val (first (first %))))) (second %)])
                                      (first (second n)))))
      :List (map first (first (second n)))
      n)
    n))


(defn reduce-collections [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (vector? %)
                   (or
                    (= (first %) :Set)
                    (= (first %) :Map)
                    (= (first %) :List)))
             simplify-collection))


(-> "xp = {#ag:2 #bs:3} vista = (1 2 1 a b 2 a) where a = 5 b = 10 end me = [1 2 3 4]"
    parse
    clean-parse-tree
    typify
    reduce-definitions
    reduce-collections
    )
