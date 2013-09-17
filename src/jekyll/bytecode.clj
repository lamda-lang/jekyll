(ns jekyll.bytecode
  (:import (java.nio ByteBuffer ByteOrder)
           (java.lang Byte)))


;; helper functions

(defn- int2bytes
  "Returns length nbytes Byte array representation of i"
  [i nbytes]
  (.array (.putInt (.order (ByteBuffer/allocate nbytes)
                           ByteOrder/LITTLE_ENDIAN)
                   i)))

(defn- bits
  "Returns sequence of s bits for an integer n, the bit order being from most
   to least significant. If the last bit is set, n is negative."
  [n s]
  (reverse (take s (map
                    (fn [i] (bit-and 0x01 i))
                    (iterate (fn [i] (bit-shift-right i 1))
                             n)))))

(defn- bits2byte
  "Turns a sequence of zeros and ones to a Byte"
  [bit-seq]
  (let [xpos (read-string (apply str (concat "2r" (rest bit-seq))))
        signum (first bit-seq)]
    (byte (if (= signum 1)
            (- xpos 128)
            xpos))))

(defn join-byte-arrays [& byte-arrays]
  (loop [bb (ByteBuffer/allocate (apply + (map count byte-arrays)))
         i 0]
    (if (< i (count byte-arrays))
      (recur (.put bb (nth byte-arrays i))
             (inc i))
      (.array bb) )))

(defn- get-bytes [barray]
  (map identity (barray)))

(defn- get-bits [barray]
  (map #(bits % 8) barray))

(defn- compress
  "Compresses a byte-array; the last bit of each byte shows if the compressed array has reached its end"
  [barray]
  (let [bits (get-bits barray)
        degen-bytes (drop-while
                     (partial every? zero?)
                     (reverse
                      (partition 7 7 [0 0 0 0 0 0 0]
                                 (reverse (flatten (reverse bits))))))
        first-bytes (map #(bits2byte (reverse (conj % 1)))
                         (butlast degen-bytes))
        last-byte (bits2byte (reverse (conj (last degen-bytes) 0)))]
    (byte-array (conj (vec first-bytes) last-byte))))

(defn- correct-bound [b]
  (if (= b "..") (nil2bin) b))

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


;; byte array representations for floats

(defn- f64
  "Returns max length 8 byte array representation of f"
  [f]
  (compress (.array (.putDouble (.order (ByteBuffer/allocate 8)
                                        ByteOrder/LITTLE_ENDIAN)
                                f))))


;; byte array representation of string  --> check!

(defn- utf32 [str]
  (apply join-byte-arrays (map (comp i32 int) str)))

(defn- ascii [str]
  (apply join-byte-arrays (map (comp i8 int) str)))



;;
;; MAIN - type conversion to bytecode
;;


(defn true2bin []
  [(i8 0)])

(defn false2bin []
  [(i8 1)])

(defn case2bin [case]
  (let [type (i8 2)
        [arg & cases] case
        case-count (i32 (count cases))]
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
        length (i32 (count elements))]
    [type length elements]))

(defn float2bin [float]
  (let [type (i8 7)
        value (f64 float)]
    [type value]))

(defn id2bin [chars]  ;; codepoints?
  (let [type (i8 8)
        length (i8 (count chars))
        content (ascii chars)]
    [type length content]))

(defn int2bin [integer]
  (let [type (i8 9)
        value (i64 integer)]
    [type value]))

(defn lambda2bin [lambda]
  (let [type (i8 10)
        [args value] lambda
        arity (i8 (count args))]
    [type value arity args]))

(defn list2bin [elements]
  (let [type (i8 11)
        length (i32 (count elements))]
    [type length elements]))

(defn map2bin [elements]
  (let [type (i8 12)
        length (i32 (count elements))]
    [type length elements]))

(defn module2bin [content]  ;; open
  (let [type (i8 13)
        length (i32 (count content))]
    [type length content]))

(defn nil2bin []
  [(i8 14)])

(defn protocol2bin [signatures]
  (let [type (i8 15)
        length (i32 (count signatures))]
    [type length signatures]))

(defn rng2bin [lower upper]
  (let [start (correct-bound lower)
        end (correct-bound upper)]
    [(i8 16) start end]))

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
        content (utf32 str)]
    [type length content]))

(defn token2bin [token]
  (let [str (rest (str token))
        type (i8 20)
        length (i8 (count str))
        content (ascii token)]
    [type length content]))

(defn type2bin [members]
  (let [type (i8 21)
        length (i32 (count members))]
    [type length members]))

(defn when2bin [conditions]
  (let [type (i8 22)
        cond-count (i32 (count conditions))]
    [type cond-count conditions]))
