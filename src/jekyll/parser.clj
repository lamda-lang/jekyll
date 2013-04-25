(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]]
        [jekyll.zip]))


(def grammar
  {:_* '(* :Whitespace)
   :Whitespace '(| \newline \return \tab \space)
   :Module [ :_* '(* [:Definition :_* ]) :$]
   :Definition [ :Identity :_* \= :_* :Expression :_* '(* :Scope)]
   :Scope [:Where :_* '(* [:Definition]) :_* :End :_*]
   :Identity [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression '(| :Identifier :Range :Float :Integer :String :Nil :Boolean :Application :Lambda :List :Map :Set :Identity) ; order is relevant
   :Application ['(| :Identity :Lambda) \( :_* '(* [:Expression :_*])]
   :Lambda [\( :_* '(* [:Identity :_*]) \: :_* '(* :Expression :_*) \)]
   :Integer [ :Digit '(* :Digit)]
   :Float [:Digit '(* :Digit) :Dot :Digit '(* :Digit)]
   :Dot \.
   :Range ['(| :Float :Integer (* :Digit)) \. \. '(| :Float :Integer (* :Digit))]
   :String [\" '(* (| :Char :SpecialChar :Digit)) \"]
   :Identifier [\# :Identity]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")
   :Where (pegs "where")
   :End (pegs "end")

   :List [\[ :_* '(* [:Expression :_*]) \]]
   :Map [\{ :_* '(* [:Identifier :_* \: :_* :Expression :_*]) \}]
   :Set [\( :_* '(* [:Expression :_*]) \)]}

  )


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
      (= x \#)
      (= x \{)
      (= x \})
      (= x \()
      (= x \))
      (= x \[)
      (= x \])
      (= x \:)
      ))


(defn clean-parse-tree [parse-tree]
  (first (get (tree-remove (universal-zip parse-tree)
                           parsing-artifact?) :Module)))


(comment
  (-> " x = false \n y = \"Hello World!\" \n lambada = (a b: plus(a b))"
      parse
      clean-parse-tree))
