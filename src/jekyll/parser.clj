(ns jekyll.parser
  (:require [instaparse.core :as insta]))

(def parser
  "Create parser with given grammar file"
  (insta/parser "doc/grammar"))

(defn parse [s]
  "Parse with our shiny new grammar."
  (insta/parse parser s))

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
(parse "set = (x for x:y if true )")
(parse "main = do a = 5 b = 10 c = \"string\" end")
(parse "x = b where b = c where c = 5 end end")
(parse "x = y where y = 2 end z = 5")
(parse "string = \"string \"")
