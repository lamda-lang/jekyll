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


(defrecord Range [start end])

(defn rangify [start end]
  (let [nstart (cond (:Integer start) (. Integer parseInt (:Integer start))
                     (:Float start) (. Float parseFloat (:Float start))
                     :else nil)
        nend (cond (:Integer end) (. Integer parseInt (:Integer end))
                   (:Float end) (. Float parseFloat (:Float end))
                   :else nil)]
    (Range. nstart nend)))


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
          :Range (rangify (correct-bound (first v)) (correct-bound (last v)))
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


(defn reduce-lambda [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (vector? %)
                   (or
                    (= (first %) :Application)
                    (= (first %) :Lambda)))
             simplify-collection))


(defn lisp-apply [reduced-parse-tree]
  (tree-edit (universal-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Application))
             (fn [matched n]
               (let [[_ [sym args]] n]
                 `(apply ~sym ~(into [] args))))))


(defn lisp-eval [reduced-parse-tree]
  (tree-edit (universal-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Lambda))
             (fn [matched n]
               (let [[_ def] n
                     body (last def)
                     args (if (= 1 (count def)) [] (first def))]
                 `(fn ~(into [] args) ~body)))))


(def plus +)


((eval (get (-> "tfn = (a b: plus(a b))"
                 parse
                 clean-parse-tree
                 typify
                 reduce-definitions
                 reduce-collections
                 reduce-lambda
                 lift-single-element-vectors
                 lisp-apply
                 lisp-eval
                 ) (symbol "tfn"))) 1 2)
