(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]] ))


(def grammar
  {:_* '(* :Whitespace)
   :Whitespace '(| \newline \return \tab \space)
   :Namespace [ :_* '(* :Definition '(| :Whitespace :$) :_*) :$]
   :Definition [ :Identity :_* \= :_* :Expression ]
   :Identity [ :Symbol ]
   :Symbol [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression ['(| :Value :Symbol )]
   :Value '(| :Keyword :Nummeric :String :Nil :Boolean :List :Map :Set)
   :Nummeric '(| [\. (| [:Digit (* :Digit)]
                        [\. :Digit (* :Digit)])]
                 [:Digit (* :Digit) (| [\. (| [\. (* :Digit)]
                                              (* :Digit))]
                                        (* :Digit))])
   :String [\" '(* (| :Char :SpecialChar)) \"]
   :Keyword [\# :Symbol]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")

   :List [\[ '(* [:_* :Expression :_*]) \]]
   :Map [\{ '(* [:_* :Keyword :_* \: :_* :Expression :_*]) \}]
   :Set [\( '(* [:_* :Expression :_*]) \)]})


(defn parse
  "Input string s."
  [s]
  (pegasus :Namespace grammar (wrap-string s)))


(comment
  (parse "x = y")
  (parse "s = \"Hello world!\"")
  (parse "k = #keyword")

  (parse "a = 42")
  (parse "pi = 3.141")
  (parse "b = .12345")
  (parse "UglyLongCaseName = 1.")

  (parse "t = true")
  (parse "f = false")
  (parse "n = nil")

  (parse "r1 = 10..99")
  (parse "r2 = ..512")
  (parse "r3 = 10..")

  (parse "s1 = (1 2)")
  (parse "s2 = ( #k )")

  (parse "m1 = {#k1:v1}")
  (parse "m2 = {}")
  (parse "m3 = { #key: \"Value\" #key2 :3}")

  (parse "l1 = [1..]")
  (parse "l2 = [1 3 \"test\" #huhu 3.141]")
  (parse "l3 = []"))
