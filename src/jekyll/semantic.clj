(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
  (:require [clojure.zip :as zip]))


(defn stringify-token [t]
  (do (println (filter list? t))
      (apply str (flatten t))))


(defn java-value [matched n]
  (if matched
    (let [[k v] (first (:Value n))]
      (case k
        :String (stringify-token (map vals (second v)))
;        :Nummeric (. Integer parseInt (stringify-token (map vals v))) ; only works for single digit atm.
        :Boolean (let [[bk bv] (first v)] (= :True bk))
        :Nil nil
        n))
    n))


(defn typify [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (map? %) (contains? % :Value))
             java-value))


(comment
  (-> "s = true \n h = \"Hello sailor!\" \n numBer=333"
      parse
      clean-parse-tree
      typify))
