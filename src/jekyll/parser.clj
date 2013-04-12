(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]]
        [jekyll.zip]))


(def grammar
  {:_* '(* :Whitespace)
   :Whitespace '(| \newline \return \tab \space)
   :Module [ :_* '(* [:Definition :_* ])]
   :Definition [ :Identity :_* \= :_* :Expression ]
   :Identity [ :Symbol ]
   :Symbol [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression ['(| :Value :Symbol )]
   :Value '(| :Identifier :Numeric :String :Nil :Boolean :List :Map :Set)
   :Integer [ :Digit '(* :Digit)]
   :Float [ :Integer \. :Integer]
   :Range ['(| :Float :Integer (* :Digit)) \. \. '(| :Float :Integer (* :Digit))]
   :Numeric '(| :Range :Float :Integer )
   :String [\" '(* (| :Char :SpecialChar)) \"]
   :Identifier [\# :Symbol]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")

   :List [\[ :_* '(* [:Expression :_*]) \]]
   :Map [\{ :_* '(* [:Keyword :_* \: :_* :Expression :_*]) \}]
   :Set [\( :_* '(* [:Expression :_*]) \)]})


(defn parse
  "Input string s."
  [s]
  (pegasus :Module grammar (wrap-string s)))


(defn parsing-artifact? [x]
  (or (and (map? x) (or (contains? x :_*)
                        (contains? x :_+)))
      (= x '())
      (= x :$)))


(defn clean-parse-tree [parse-tree]
  (tree-remove (universal-zip parse-tree)
               parsing-artifact?))


(comment
  (-> " x = false \n y = \"Hello World!\" \n"
      parse
      clean-parse-tree))
