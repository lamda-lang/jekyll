(ns jekyll.file-reader
  (:use clojure.core))

(defn to-string
  [filename]
  (slurp filename))
  