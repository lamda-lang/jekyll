(ns jekyll.parser
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "Module = <_*> Definition* <_*>
    Definition = Identity <_*> <'='> <_*> Expression <_*> Scope*
    Scope = <'where'> <_*> (Definition <_*>)* <'end'> <_*>
    Identity = !('nil' | 'true' | 'false' | 'when' | 'case') #'[A-Za-z_]\\w*'
    Expression = Identifier | Range | Integer | String | Nil | Boolean | Application | Lambda | List | Map | Set | Identity | Condition | Case | Property | Subscript | Float
    Expressions = (Expression <_+>)* Expression
    KeyValue = Expression <_*> <':'> <_*> Expression
    KeyValues = (KeyValue <_+>)* KeyValue
    Application = ( Identity | Application | Lambda ) <'('>  <_*> [Expressions <_*>] <')'>
    Lambda = <'('> <_*> Args <':'> <_*> Expressions <_*> <')'>
    Condition = <'when'> <_*> (KeyValue <_*>)* <'end'>
    Case = <'case'> <_*> Identity <_+> (Expression <_*> [Guard <_*>] <':'> <_*> Expression <_*>)* <'end'>
    Property = Expression <'.'> Expression
    Guard = <'if'> <_*> Expressions
    Subscript = Expression <'['> <_*> Expression <_*> <']'>
    ListComprehension = Expression <_*> <'for'> <_*> KeyValue <_*> [Guard]
    MapComprehension = KeyValue <_*> <'for'> <_*> KeyValue <_*> [Guard]
    Args = (Identity <_*>)*
    Float = #'-?\\d+.\\d+'
    Range = ( Expression | Epsilon ) '..' ( Expression | Epsilon )
    Integer = #'-?\\d+'
    String = <'\\\"'> #'[^\"]*' <'\\\"'>
    Identifier = <'#'> #'\\w+'
    Nil = <'nil'>
    Boolean = 'true' | 'false'
    List = <'['> <_*> ( Expressions | ListComprehension )? <_*> <']'>
    Map =  <'{'> <_*> ( KeyValues   | MapComprehension  )? <_*> <'}'>
    Set =  <'('> <_*> ( Expressions | ListComprehension )? <_*> <')'>
    _ = #'\\s'"))



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
