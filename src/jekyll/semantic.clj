(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
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
    (let [[k & elems] n]
      (case k
        :Set (into #{} elems)
        :Map (apply array-map elems)
        :List elems
        n))
    n))


(defn reduce-collections [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (vector? %)
                   (or
                    (= (first %) :Set)
                    (= (first %) :Map)
                    (= (first %) :List)))
             simplify-collection))


(defn lisp-apply [reduced-parse-tree]
  (tree-edit (zip/vector-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Application))
             (fn [matched n]
               (let [[_ sym & args] n]
                 `(apply ~sym ~(into [] args))))))


(defn lisp-eval [reduced-parse-tree]
  (tree-edit (zip/vector-zip reduced-parse-tree)
             #(and (vector? %)
                   (= (first %) :Lambda))
             (fn [matched n]
               (let [[_ [_ & args] [& body]] n]
                 `(fn ~(into [] args) ~body)))))


(def plus +)

(defn third [col] (get col 2))

((-> "tfn = (a b: plus(a b) )"
      parse
      typify
      lisp-apply
      lisp-eval
      second
      third
      eval
      ) 1 2)
