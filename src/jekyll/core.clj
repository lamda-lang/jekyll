(ns jekyll.core
  (:use [jekyll.parser :only [parse]]))


(def stages
  "Stages of compilation."
  [slurp parse cleanup])


(defn- index-of [item coll]
  (count (take-while (partial not= item) coll)))


(defn- subrange
  "Returns a inclusive range between start
   and end in a vector."
  [start end]
  (subvec stages
          (index-of start stages)
          (inc (index-of end stages))))


(defn call-stage
  "Call a single stage on input."
  [input stage]
  (stage input))


(defn call-stages
  "Call compilation stages from start-stage to end-stage."
  [start-stage end-stage input]
  (reduce call-stage input (subrange start-stage end-stage)))


(defn all-stages
  "Run all stages and spit out bytecode."
  [input]
  (reduce call-stage input stages))

(all-stages "doc/value_sample")
