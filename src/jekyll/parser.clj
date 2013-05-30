(ns jekyll.parser
  (:require [instaparse.core :as insta]))

(def parser
  "Create parser with given grammar file"
  (insta/parser "doc/grammar"))


(defn parse [s]
  "Parse with our shiny new grammar."
  (insta/parse parser s))


(comment
  (-> "lambada = (a b: id(a b))(1)(2)"
      parse))

(parse "lambada = (a b: id(a b))(1)(2)")
(parse "condition = when true: 1 false: 0 end")
(parse "ds = case variable \"gfd\" : dosomething \"gfrdse\" : dosomethingelse end")
(parse "condition = case name
       \"Johannes\" if true: \"Auer\"
       \"Susan\" if false: \"Ismail\"
       end")
(parse "grfsdg = a.b.c.vfds")
(parse "gds = [gs(3) g (3)]")
(parse "gds = [gs[3] g [3]]")
(parse "map = { i:j for x:y if true }")
