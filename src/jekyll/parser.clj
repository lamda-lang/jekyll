(ns jekyll.parser
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser
   "Module = <_*> Definition* <_*>
    Definition = Identity <_*> <'='> <_*> Expression <_*> Scope*
    Scope = <'where'> <_*> (Definition <_*>)* <'end'> <_*>
    Identity = !('nil' | 'true' | 'false') #'[A-Za-z_]\\w*'
    Char = #'[A-Za-z_]'
    Expression = Identifier | Range | Float | Integer | String | Nil | Boolean | Application | Lambda | List | Map | Set | Identity
    Application = ( Identity | Application | Lambda ) <'('>  <_*> (Expression <_*>)* <')'>
    Lambda = <'('> <_*> Args <':'> <_*> (Expression <_+>)* Expression <_*> <')'>
    Args = (Identity <_*>)*
    Float = #'-?\\d+' '.' #'\\d+'
    Range = ( Float | Integer | Epsilon ) '..' ( Float | Integer | Epsilon )
    Integer = #'-?\\d+'
    String = <'\\\"'> #'[^\"]*' <'\\\"'>
    Identifier = <'#'> #'\\w+'
    Nil = <'nil'>
    Boolean = 'true' | 'false'
    List = <'['> <_*> (Expression <_*>)* <']'>
    Map = <'{'> <_*> ( Expression <_*> <':'> <_*> Expression <_*> )* <'}'>
    Set = <'('> <_*> (Expression <_*>)* <')'>
    _ = #'\\s'"))


(defn parse [s]
  "Parse with our shiny new grammar."
  (insta/parse parser s))


(comment
  (-> "lambada = (a b: id(a b))(1)(2)"
      parse))
