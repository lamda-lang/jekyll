(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]]
        [jekyll.semantic]
        [jekyll.zip]))


(def grammar
  {:_* '(* :Whitespace)
   :Whitespace '(| \newline \return \tab \space)
   :Module [ :_* '(* [:Definition :_* ])]
   :Definition [ :Identity :_* \= :_* :Expression :_* '(* :Scope)]
   :Scope [:Where :_* '(* :Definition) :_* :End :_*]
   :Identity [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression '(| :Identifier :Range :Float :Integer :String :Nil :Boolean :List :Map :Set :Identity)
   :Integer [ :Digit '(* :Digit)]
   :Float [ :Integer \. :Integer]
   :Range ['(| :Float :Integer (* :Digit)) \. \. '(| :Float :Integer (* :Digit))]
   :String [\" '(* (| :Char :SpecialChar)) \"]
   :Identifier [\# :Identity]
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
  (or (and (map? x) (or (contains? x :_*
                                   )
                        (contains? x :_+)))
      (= x '())
      (= x :$)
      (= x \=)))


(defn clean-parse-tree [parse-tree]
  (tree-remove (universal-zip parse-tree)
               parsing-artifact?))


(comment
  (-> " x = false \n y = \"Hello World!\" \n"
      parse
      clean-parse-tree))

(def tree
  (-> "x = \"asfdadf\" y = true"
              parse
              clean-parse-tree))

(map #(val (first %)) (tree-find (universal-zip tree) #(and (map? %) (contains? % :Definition))))

(-> "x = y where y = 2 a = 1 end y = 7 a = 5"
    parse
    clean-parse-tree)
