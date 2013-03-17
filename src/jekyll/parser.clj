(ns jekyll.parser
  (:use [com.lithinos.amotoen.core :only [pegs lpegs pegasus wrap-string]] ))


(def grammar
  {:_* '(* (| \newline \return \tab \space))
   :Namespace [ :_* '(* :Definition :_*) :$]
   :Definition [ :Identity :_* \= :_* :Expression :_* ]
   :Identity [ :Symbol ]
   :Symbol '(* :Char)
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_")
   :ExtendedChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression '(| :Value :Range :Symbol)
   :Value '(| :Keyword :Integer :Float :String :Nil :Boolean)
   :Range '(| [ \. \. :Integer]
              [:Integer \. \. :Integer]
              [:Integer \. \.])
   :String [\" '(* (| :Char :ExtendedChar)) \"]
   :Keyword [\# :Symbol]
   :Float ['(* :Digit) \. :Digit '(* :Digit)]
   :Integer [:Digit '(* :Digit)]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")})

(name :$)

(defn parse
  "Input string s."
  [s]
  (pegasus :Namespace grammar (wrap-string s)))


(comment
  (parse "x = y")
  (parse "a = 42")
  (parse "s = \"Hello world!\"")
  (parse "pi = 3.141")
  (parse "b = .12345")
  (parse "k = #keyword")
  (parse "t = true")
  (parse "f = false")
  (parse "n = nil")
  (parse "r1 = 1..99"))
