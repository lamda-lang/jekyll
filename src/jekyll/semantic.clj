(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser]
        [clojure.inspector :only [inspect-tree]])
  (:require [clojure.zip :as zip]))



(defn correct-bound [b]
  (if (= b "..")
    nil
    (let [[k v] b] {k v})))


(defrecord Range [start end])


(defn rangify [start end]
  (let [nstart (cond (:Integer start) (. Integer parseInt (:Integer start))
                     (:Float start) (. Float parseFloat (:Float start))
                     :else nil)
        nend (cond (:Integer end) (. Integer parseInt (:Integer end))
                   (:Float end) (. Float parseFloat (:Float end))
                   :else nil)]
    (Range. nstart nend)))


(defn collapse-expressions [matched n]
  (if matched
    (let [[_ & exps] n]
      (into [] exps))))


(defn collapse [clean-parse-tree]
  (tree-edit (zip/vector-zip clean-parse-tree)
             #(and (vector? %) (or
                                (= (first %) :Expressions)))
             collapse-expressions))


#_(collapse true [:Expressions [1 2] [3 4]])

(defn typify-definition [matched n]
  (if matched
    (if (= :Expression (first n))
      (let [[k & vs] (second n)
            [v] vs]
        (case k
          :String v
          :Identity (symbol v)
          :Identifier (keyword v)
          :Integer  (. Integer parseInt v)
          :Float (. Float parseFloat v)
          :Range (rangify (correct-bound (first vs)) (correct-bound (last vs)))
          :Boolean (= v "true")
          :Nil nil
          (second n)))
      (symbol (second n)))
    n))


(defn typify [clean-parse-tree]
  (tree-edit (zip/vector-zip clean-parse-tree)
             #(and (vector? %) (or
                                (= (first %) :Identity)
                                (= (first %) :Expression)))
             typify-definition))


(defn simplify-collection [matched n]
  (if matched
    (let [[k elems] n]
      (case k
        :Set (into #{} elems)
        :Map (apply array-map elems)
        :List (into [] elems) ; might not match lamda list semantics
        n))
    n))


(defn reduce-collections [clean-parse-tree]
  (tree-edit (zip/vector-zip clean-parse-tree)
             #(and (vector? %)
                   (or
                    (= (first %) :Set)
                    (= (first %) :Map)
                    (= (first %) :List)))
             simplify-collection))


(defn lisp-apply [reduced-parse-tree]
  (tree-edit (zip/vector-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Result))
             (fn [matched n]
               (let [[_ sym & [args]] n]
                 `(apply ~sym ~args)))))


(defn lisp-eval [reduced-parse-tree]
  (tree-edit (zip/vector-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Lambda))
             (fn [matched n]
               (let [[_ [_ & args] [& body]] n]
                 `(fn ~(into [] args) ~body)))))

(defn lisp-let [reduced-parse-tree]
  (tree-edit (zip/vector-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Definitions))
             (fn [matched [_ & defs]]
               `(let ~(vec (reduce concat (map rest defs)))))))



(def plus +)

#_(-> "tfn = (a b: map(plus [1 2 a] (10 11 12))) res = tfn(1 2)"
      parse
      inspect-tree)

#_(-> "tfn = (a b: map(plus [1 2 a] (10 11 12))) res = tfn(1 2)"
      parse
      collapse
      typify
      reduce-collections
      lisp-apply
      lisp-eval
      lisp-let
      eval
      )
