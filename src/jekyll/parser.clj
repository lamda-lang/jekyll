(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]]
        [jekyll.semantic :only [typify]]
        [jekyll.zip]))


(def grammar
  {:Whitespace '(| \newline \return \tab \space)
   :_* '(* :Whitespace)
   :_+ [:Whitespace :_*]
   :Namespace [:_* '(* [:Definition (| :_+ :$)]) :$]
   :Definition [ :Identity :_* \= :_* :Expression ]
   :Identity [ :Symbol ]
   :Symbol [:Char '(* (| :Char :Digit))]
   :Char (lpegs  '| "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")
   :SpecialChar (lpegs '| " -!@#$%^&*()+=[]{}\\|/?.,;:'<>")
   :Expression ['(| :Value :Identity )]
   :Value '(| :Identifier :Nummeric :String :Nil :Boolean :List :Map :Set)
   :Nummeric '(| [\. [\. :Digit (* :Digit)]]
                 [:Digit (* :Digit) (| [\. (| [\. (* :Digit)]
                                              [:Digit (* :Digit)])]
                                       (* :Digit))])
   :String [\" '(* (| :Char :SpecialChar)) \"]
   :Identifier [\# :Symbol]
   :Digit (lpegs '| "0123456789")
   :Nil (pegs "nil")
   :Boolean '(| :True :False)
   :True (pegs "true")
   :False (pegs "false")

   :List [\[ '(* [:_* :Expression :_*]) \]]
   :Map [\{ '(* [:_* :Expression :_* \: :_* :Expression :_*]) \}]
   :Set [\( '(* [:_* :Expression :_*]) \)]})


(defn parse
  "Input string s."
  [s]
  (pegasus :Namespace grammar (wrap-string s)))


(defn clean-parse-tree [parse-tree]
  (tree-remove (universal-zip parse-tree) parsing-artifact?))


(defn parsing-artefact? [x]
  (or (and (map? x) (or (contains? x :_*)
                        (contains? x :_+)))
      (= x '())))


(comment
  (-> (clean-parse-tree (parse " x = 1 \n y = \"Hello World!\" \n"))))
