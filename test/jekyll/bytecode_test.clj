(ns jekyll.bytecode-test
  (:use clojure.test
        jekyll.parser
        jekyll.semantic
        jekyll.bitsnbytes
        midje.sweet))

;; helper functions

(defn def2barrays
  "Returns a sequence of 2 byte arrays: name and value of a definition"
  [def-str]
  (map join-bytes
       (rest (first (second
                     (binarize-expressions (collapse (parse def-str))))))))

(defn definition-value
  "Returns bit sequence of the value of a definition"
  [def-str]
  (map (partial apply str) (get-bits (second (def2barrays def-str)))))


;;
;; MAIN - discuss byteorder, fixed length with ascii, etc. !!!
;;

(fact (definition-value "bool = true")  => '("00000000")) ;0
(fact (definition-value "bool = false") => '("00000010")) ;1
(fact (definition-value "val = nil")    => '("00011100")) ;14

(fact (definition-value "integer = 1")  => '("00010010" "00000010")) ;9
;;(fact (definition-value "float = 1.0") => '("00010010" "00000010")) ;7

(fact (definition-value "string = \"ab\"") => '("00100110" "00000100" "11000010" "11000100")) ;19
(fact (definition-value "token = #ab")     => '("00101000" "00000100" "01100001" "01100010")) ;20
(fact (definition-value "identifier = ab") => '("00010000" "00000010" "00000100" "01100001" "01100010")) ;8


(definition-value "val = #ab")
(parse "val = #ab")
