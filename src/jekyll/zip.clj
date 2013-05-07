(ns jekyll.zip
  (:require [clojure.zip :as zip]))


(defn tree-remove [zipper matcher]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/remove loc)))
        (recur (zip/next loc))))))


(defn tree-edit [zipper matcher editor]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (do (println (zip/node loc) " results in " matcher-result)
          (recur (zip/next (zip/edit loc (partial editor matcher-result)))))
        (recur (zip/next loc))))))


(defn tree-find
  [zipper matcher]
  (loop [loc zipper
         node-list '()]
    (if (zip/end? loc)
      node-list
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next loc) (conj node-list (zip/node loc)))
        (recur (zip/next loc) node-list)))))
