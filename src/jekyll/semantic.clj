(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
  (:require [clojure.zip :as zip]))


(defn stringify [t]
  (apply str (flatten (map vals (flatten t)))))


;(defn stringify-nums []

  (defn reduce-tree [matched n]
    (if matched
      (let [mv (first (val (first n)))
            [k v] mv]
        (case k
          :String {:String (stringify (second v))}
          :Symbol (stringify v)
          :Integer {:Integer (stringify v)}
          :Float {:Float (stringify v)}
          :Range {:Range [(stringify (val (first (first v)))) (stringify (val (first (last v))))]}
          :Boolean {:Boolean (let [[bk bv] (first v)] (= :True bk))}
          :Nil nil
          n))
      n))


  (defn typify [clean-parse-tree]
    (tree-edit (universal-zip clean-parse-tree)
               #(and (map? %) (or
                               (contains? % :Identity)
                               (contains? % :Expression)))
               reduce-tree))


  (-> "s = true \n h = \"Hello sailor!\" \n numBer=333 \n float = 2.34 \n range = 1..8 "
      parse
      clean-parse-tree
      typify
      )
