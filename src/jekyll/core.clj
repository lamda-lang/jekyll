(ns jekyll.core
  (:use jekyll.parser))

(defn call-stage
  [stage input])

(defn call-stages
  [start-stage end-stage input])

(defn all-stages
  [input])

(def stages
  [])