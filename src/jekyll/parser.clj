(ns jekyll.parser
  (:require [instaparse.core :as insta]))

(def parser
  "Create parser with given grammar file"
  (insta/parser "doc/grammar"))

(defn parse [s]
  "Parse with our shiny new grammar."
  (insta/parse parser s))
