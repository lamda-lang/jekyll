(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]]
        [jekyll.zip]))


(def grammar
  {:_* '(* :Whitespace)
   :Whitespace '(| \newline \return \tab \space)
   :Module [ :_* '(* [:Definition :_* ] :$)]
   :Definition [ :Identity :_* \= :_* :Expression :_* '(* :Scope)]
   :Scope [:Where :_* '(* :Definition) :_* :End :_*]
   :Identity '(| :Symbol)
   :Symbol [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression '(| :Identifier :Range :Float :Integer :String :Nil :Boolean :List :Map :Set :Symbol) ; order is relevant
   :Integer [ :Digit '(* :Digit)]
   :Float [ :Digit '(* :Digit) :Dot :Digit '(* :Digit)]
   :Dot \.
   :Range ['(| :Float :Integer (* :Digit)) \. \. '(| :Float :Integer (* :Digit))]
   :String [\" '(* (| :Char :SpecialChar :Digit)) \"]
   :Identifier [\# :Symbol]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")
   :Where (pegs "where")
   :End (pegs "end")

   :List [\[ :_* '(* [:Expression :_*]) \]]
   :Map [\{ :_* '(* [:Keyword :_* \: :_* :Expression :_*]) \}]
   :Set [\( :_* '(* [:Expression :_*]) \)]})


(defn parse
  "Input string s."
  [s]
  (pegasus :Module grammar (wrap-string s)))


(defn parsing-artifact? [x]
  (or (and (map? x) (or (contains? x :_*)
                        (contains? x :_+)
                        (contains? x :Where)
                        (contains? x :End)))
      (= x '())
      (= x :$)
      (= x \=)
      (= x \")
      ))


(defn clean-parse-tree [parse-tree]
  (tree-remove (universal-zip parse-tree)
               parsing-artifact?))


(comment
  (-> " x = false \n y = \"Hello World!\" \n"
      parse
      clean-parse-tree))
