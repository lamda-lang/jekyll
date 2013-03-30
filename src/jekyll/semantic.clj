(ns jekyll.semantic
  (:use [jekyll.zip])
  (:require [clojure.zip :as zip]))


(defn typify [clean-parse-tree]
  (tree-edit clean-parse-tree type add-meta))


(defn type [x]
  "Check parsed value if proper for type and return type."
  (if-not (map? x)
    nil
    {:type
     (cond
      (contains? x :Nummeric) (cond (not (some (:Nummeric x))) :Nummeric)
      (contains? x :String) :String)}))


(defn add-meta [meta x]
  (do
    (println "adding meta " meta " for "x)
    (with-meta x
      (merge-with assoc (or (meta x) {}) meta))))


(comment
  (meta (typify (universal-zip {:Nummeric 3}))))
