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
  (map (partial apply str)
       (get-bits (second (def2barrays def-str)))))


;;
;; MAIN - discuss byteorder, fixed length with ascii, etc. !!!
;;

(fact (definition-value "bool = true")  => '("00000000")) ;0
(fact (definition-value "bool = false") => '("00000010")) ;1
(fact (definition-value "val = nil")    => '("00011100")) ;14

(fact (definition-value "integer = 1")  => '("00010010" "00000010")) ;9 1
;;(fact (definition-value "float = 1.0")  => '("00010010" "00000010")) ;7
(fact (definition-value "range = ..10") => '("00100000" "00011100" "00010010" "00010100")) ;16 14 9 10

(fact (definition-value "string = \"ab\"") => '("00100110" "00000100" "11000010" "11000100")) ;19 2 a b
(fact (definition-value "token = #ab")     => '("00101000" "00000100" "01100001" "01100010")) ;20 2 a b
(fact (definition-value "identifier = ab") => '("00010000" "00000010" "00000100" "01100001" "01100010")) ;8 1 2 a b

(fact (definition-value "list = [1 \"ab\"]")
      => '("00010110" "00000100" "00010010" "00000010" "00100110" "00000100" "11000010" "11000100"))
                                        ;11 2 9 1 19 2 a b
(fact (definition-value "set = (1 \"ab\")")
      => '("00100100" "00000100" "00010010" "00000010" "00100110" "00000100" "11000010" "11000100"))
                                        ;18 2 9 1 19 2 a b
(fact (definition-value "map = {#ab:1}")
      => '("00011000" "00000010" "00101000" "00000100" "01100001" "01100010" "00010010" "00000010"))
                                        ;12 1 20 2 a b 9 1

(fact (definition-value "res = f(1)")
      => '("00100010" "00010000" "00000010" "00000010" "01100110" "00000010" "00010010" "00000010"))
                                        ;17 8 1 1 f 1 9 1
(fact (definition-value "val = type ab end")
      => '("00101010" "00000010" "00010000" "00000010" "00000100" "01100001" "01100010"))
                                        ;21 1 8 1 2 a b
(fact (definition-value "lambda = (x:1)")
      => '("00010100" "00010010" "00000010" "00000010" "00010000" "00000010" "00000010" "01111000"))
                                        ;10 9 1 1 8 1 1 x
(fact (definition-value "cond = when true:1 false:0 end")
      => '("00101100" "00000100" "00000000" "00010010" "00000010" "00000010" "00010010" "00000000"))
                                        ;22 2 0 9 1 1 9 0
(fact (definition-value "cond2 = case x 1 if true: true 2: false end")
      => '("00000100" "00010000" "00000010" "00000010" "01111000" "00000100" "00010010" "00000010" "00000000" "00000000" "00010010" "00000100" "00011100" "00000010"))
                                        ;2 8 1 1 x 2 9 1 0 0 9 2 14 1
(fact (definition-value "seq = do x = 1 end")
      => '("00001100" "00000010" "00010000" "00000010" "00000010" "01111000" "00010010" "00000010"))
                                        ;6 1 8 1 1 x 9 1
(fact (definition-value "prtcl = protocol f(x) end")
      => '("00011110" "00000010" "00010000" "00000010" "00000010" "01100110" "00000010"))
                                        ;15 1 8 1 1 f 1
