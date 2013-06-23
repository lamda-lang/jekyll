(ns jekyll.bytecode
  (:import (java.nio ByteBuffer ByteOrder)
           (java.lang Byte))
  (:require [clojure.math.numeric-tower :as math]))


;; helper functions

(defn intToBytes
  "Returns length nbytes Byte array representation of i"
  [i nbytes]
  (.array (.putInt (.order (ByteBuffer/allocate nbytes)
                           ByteOrder/LITTLE_ENDIAN)
                   i)))

(defn bits [n s]
  "Returns sequence of s bits for an integer n, the bit order being from most
   to least significant. If the last bit is set, n is negative."
  (reverse (take s (map
                    (fn [i] (bit-and 0x01 i))
                    (iterate (fn [i] (bit-shift-right i 1))
                             n)))))

(defn bits2byte
  "Turns a sequence of zeros and ones to a Byte"
  [bit-seq]
  (let [xpos (read-string (apply str (concat "2r" (rest bit-seq))))
        signum (first bit-seq)]
    (byte (if (= signum 1)
            (- xpos 128)
            xpos))))

(defn var-byte-array
  "Returns length nbytes Byte array representation of i
   (nbytes <= capacity)"
  [i nbytes capacity]
  (if (and (<= nbytes capacity) (pos? nbytes))
    (let [len (* nbytes 7)
          all-bytes-used (= nbytes capacity)
          degen-bytes (partition 7 (bits i (if all-bytes-used (inc len) len)))

          first-bytes (map #(bits2byte (concat % [1]))
                          (take (dec nbytes) degen-bytes))
          last-byte (if all-bytes-used
                      (bits2byte (concat (last degen-bytes)
                                         [(if (even? i) 0 1)]))
                      (bits2byte (concat (last degen-bytes) [0])))]
      (byte-array (conj (vec first-bytes) last-byte)))
    [] ))


;; byte array representations for integers

(defn i8fle
  "Returns length 1 byte array representation of i"
  [i]
  (byte-array [(byte i)]))

(defn i32fle
  "Returns length 4 byte array representation of i"
  [i]
  (intToBytes i 4))

(defn i64fle
  "Returns length 8 byte array representation of i"
  [i]
  (intToBytes i 8))

(defn i32vle
  "Returns byte array representation of i (max length 4)"
  [i]
  (cond
   (and (>= i -64)        (< i 64))        (var-byte-array i 1 4)
   (and (>= i -8192)      (< i 8192))      (var-byte-array i 2 4)
   (and (>= i -1048576)   (< i 1048576))   (var-byte-array i 3 4)
   (and (>= i -268435456) (< i 268435456)) (var-byte-array i 4 4)
   :else []))


;; byte array representations for floats

(defn f64fle
  "Returns length 8 byte array representation of f"
  [f]
  (.array (.putDouble (.order (ByteBuffer/allocate 8)
                           ByteOrder/LITTLE_ENDIAN)
                      i)))

;;
;; MAIN - type conversion to bytecode
;;

(defn nilToBin []
  (i8fle 0))

(defn trueToBin []
  (i8fle 1))

(defn falseToBin []
  (i8fle 2))

(defn intToBin [integer]
  (let [type (i8fle 3)
        value (i64fle integer)
        nbytes (ByteBuffer/allocate (+ 1 8))]
    (.array (.put (.put nbytes type) value))))

(defn floatToBin [float]
  (let [type (i8fle 4)
        value (f64fle float)
        nbytes (ByteBuffer/allocate (+ 1 8))]
    (.array (.put (.put nbytes type) value))))

(defn idToBin [id l]
  (let [type (i8fle 5)
        length (18fle l)
        ;; codepoint?
        nbytes (ByteBuffer/allocate (+ 1 1))]
    (.array (.put (.put nbytes type) length))))

(defn strToBin [str l]
  (let [type (i8fle 6)
        length (i32vle l)
        ;; codepoint?
        nbytes (ByteBuffer/allocate (+ 1 1))]
    (.array (.put (.put nbytes type) length))))

(defn rngToBin [start-ind end-ind]
  (let [type (i8fle 7)
        start (i32vle start-ind)
        end (i32vle end-ind)
        nbytes (ByteBuffer/allocate (+ 1 (count start) (count end)))]
    (.array (.put (.put nbytes type) length))))

(defn setToBin [l]
  (let [type (i8fle 8)
        num-el (i32vle l)
        ;; elem-ind?
        nbytes (ByteBuffer/allocate (+ 1 (count num-el)))]
    (.array (.put (.put nbytes type) length))))


;;
;; appendix - printing functions for byte arrays
;;

(defn print-bytes [barray]
  (map #(get barray %) (range (count barray))))

(defn print-bits [barray]
  (map #(bits % 8) (print-bytes barray)))
