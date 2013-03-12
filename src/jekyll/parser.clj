(ns jekyll.parser
  (:use [com.lithinos.amotoen.core :only [pegs lpegs pegasus wrap-string]] ))


(def grammar
  {
   :Document ['(* :String) :$]
   :String (lpegs  '| "ab")
   })


(defn parse
  "Input string s."
  [s]
  (pegasus :Document grammar (wrap-string s)))


(comment
  (parse "ab"))
