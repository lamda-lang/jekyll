(ns jekyll.semantic
  (:use [jekyll.zip]
        [jekyll.parser])
  (:require [clojure.zip :as zip]))


(defn stringify-token [t]
  (do (println (filter list? t))
      (apply str (flatten t))))


(defn java-value [matched n]
  (if matched
    (let [[mk mv] (first n)]
      (case mk
        :Identity (stringify-token (map vals (flatten mv)))
        :Integer {:Integer (stringify-token (map vals (flatten mv)))}
        ;:Float {:Float (stringify-token [(val (first mv)) \. (val (last mv))])}
        :Expression (let [[k v] mv]
                      (case k
                        :String {:String (stringify-token (map vals (second v)))}
                        :Integer mv
                        :Float mv
                                        ; :Range [(first v) (last v)]
                        :Boolean (let [[bk bv] (first v)] (= :True bk))
                        :Nil nil
                        n))))
     n))


(defn typify [clean-parse-tree]
  (tree-edit (universal-zip clean-parse-tree)
             #(and (map? %) (or
                             (contains? % :Identity)
                             (contains? % :Integer)
                             (contains? % :Float)
                             (contains? % :Expression)))
             java-value))


(defn reduce-tree
  [typified-parse-tree]
  (vec (map #(:Definition (first %)) (first (:Module typified-parse-tree)))))



  (-> "s = true \n h = \"Hello sailor!\" \n numBer=333 \n float = 2.34 "
              parse
              clean-parse-tree
              typify
              )
