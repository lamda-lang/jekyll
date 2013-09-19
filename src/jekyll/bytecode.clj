(ns jekyll.bytecode
  (:use jekyll.bitsnbytes)
  (:import (java.nio ByteBuffer ByteOrder)
           (java.lang Byte)))


;; byte array representations for integers

(defn- i8
  "Returns variable length byte array representation of i (max. 8 bits)"
  [i]
  (compress (byte-array [(byte i)])))

(defn- i32
  "Returns variable length byte array representation of i (max. 32 bits)"
  [i]
  (compress (int2bytes i 4)))

(defn- i64
  "Returns variable length byte array representation of i (max. 64 bits)"
  [i]
  (compress (int2bytes i 8)))


;; byte array representations for floats      --> faulty !!!

(defn- f64
  "Returns variable length byte array representation of f (max. 64 bits)"
  [f]
  (compress (double2bytes f 8)))

;; check:
(get-bits (double2bytes 1.0 8))
(get-bits (compress (double2bytes 1.0 8)))


;; byte array representation for strings

(defn- utf32
  "Returns variable length utf-32 encoded string"
  [string]
  (map #(compress (.getBytes (str %) "utf-32le")) string))

(defn- ascii
  "Returns variable length ascii encoded string"
  [str]
  (.getBytes str "us-ascii"))

(get-bits (ascii "ab"))
(get-bits (utf32 "a"))

;; little helper for range

(defn- correct [bound]
  (if (= bound "..") (nil2bin) bound))



;;
;; MAIN - type conversion to bytecode
;;


(defn true2bin []
  (i8 0))

(defn false2bin []
  (i8 1))

(defn case2bin [case]
  (let [type (i8 2)
        [arg & css] case
        case-count (i32 (count css))
        cases (map #(let [vs (rest %)
                          match (first vs)
                          guard (if (< (count vs) 3) (nil2bin) (second (second vs)))
                          value (last vs)]
                      [match guard value])
                 css)]
    [type arg case-count cases]))

(defn list-comp2bin [comp] ; --> keyvalues? guard?
  (let [type (i8 3)
        [value kv & rmd] comp
        guard (if empty? rmd (nil2bin) rmd)]
    [type value kv guard]))

(defn map-comp2bin [comp] ; --> keyvalues? guard?
  (let [type (i8 4)
        [value kv & rmd] comp
        guard (if (empty? rmd) (nil2bin) rmd)]
    [type value kv guard]))

(defn set-comp2bin [comp] ; --> keyvalues? guard?
  (let [type (i8 5)
        [value kv & rmd] comp
        guard (if empty? rmd (nil2bin) rmd)]
    [type value kv guard]))

(defn do2bin [elements]
  (let [type (i8 6)
        length (i32 (count elements))
        elems (map #(rest (first %)) elements)]
    [type length elems]))

(defn float2bin [float]
  (let [type (i8 7)
        value (f64 float)]
    [type value]))

(defn id2bin [components]
  (let [type (i8 8)
        ncomp (i8 (count components))
        content (map #(let [comp (second %)]
                        [(i8 (count comp)) (ascii comp)]) components)]
    [type ncomp content]))

(defn int2bin [integer]
  (let [type (i8 9)
        value (i64 integer)]
    [type value]))

(defn lambda2bin [lambda]
  (let [type (i8 10)
        value (second lambda)
        args (rest (first lambda))
        arity (i8 (count args))]
    [type value arity args]))

(defn list2bin [elements]
  (let [type (i8 11)
        length (i32 (count elements))]
    [type length elements]))

(defn map2bin [elements]
  (let [type (i8 12)
        elems (map rest (rest elements))
        length (i32 (count elems))]
    [type length elems]))

(defn module2bin [content]  ;; open
  (let [type (i8 13)
        length (i32 (count content))]
    [type length content]))

(defn nil2bin []
  (i8 14))

(defn protocol2bin [signatures]
  (let [type (i8 15)
        length (i32 (count signatures))
        sign (map #(vector (second %) (i8 (count (rest (last %))))) signatures)]
    [type length sign]))

(defn rng2bin [lower upper]
  (let [type (i8 16)
        start (correct lower)
        end (correct upper)]
    [type start end]))

(defn res2bin [result]
  (let [type (i8 17)
        [target args] result
        arg-count (i8 (count args))]
    [type target arg-count args]))

(defn set2bin [elements]
  (let [type (i8 18)
        length (i32 (count elements))]
    [type length elements]))

(defn str2bin [string]
  (let [type (i8 19)
        length (i32 (count string))
        content (utf32 string)]
    [type length content]))

(defn token2bin [token]
  (let [type (i8 20)
        length (i8 (count token))
        content (ascii token)]
    [type length content]))

(defn type2bin [members]
  (let [type (i8 21)
        length (i32 (count members))]
    [type length members]))

(defn when2bin [conditions]
  (let [type (i8 22)
        cond-count (i32 (count conditions))
        conds (map rest conditions)]
    [type cond-count conds]))
