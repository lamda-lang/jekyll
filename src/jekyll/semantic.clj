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
          n))
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

(-> "s = true \n h = \"Hello sailor!\" \n numBer=a where a = 333 b=c where c = 123 end end \n float = 2.34 \n range = ..89090 "
            parse
            clean-parse-tree
            typify
            reduce-tree
            )
