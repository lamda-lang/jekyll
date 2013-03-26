(ns jekyll.parser
  (:use [jekyll.pegasus :only [pegs lpegs pegasus wrap-string]] )
  (:require [clojure.zip :as zip]))


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


(defn grammar-zip [root]
  "Zipper for parse trees."
  (let [branch? (fn [node]
                  (when node
                    (or (map? node)
                        (vector? node))))
        children (fn [node]
                   (cond
                     (nil? node) nil
                     (map? node) (seq node)
                     :else node))
        make-node (fn [node children]
                    (cond
                      (nil? node) nil
                      (vector? node) (into [] children)
                      (map? node) (let [[[k v]] children] (assoc node k v))
                      :else node))]
    (zip/zipper branch? children make-node root)))


(defn tree-remove [zipper matcher]
  (loop [loc zipper]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [matcher-result (matcher (zip/node loc))]
        (recur (zip/next (zip/remove loc)))
        (recur (zip/next loc))))))


(comment
  (tree-remove (grammar-zip (parse "x=1"))
               #(or (and (map? %) (contains? % :_*))
                    (= % '()))))
