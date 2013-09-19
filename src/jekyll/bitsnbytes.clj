(ns jekyll.bitsnbytes
  (:import (java.nio ByteBuffer ByteOrder)
           (java.lang Byte)))


(defn int2bytes
  "Returns length nbytes Byte array representation of integer i
   (nbytes must be multiple of 4)"
  [i nbytes]
  (.array (.putInt (.order (ByteBuffer/allocate nbytes)
                           ByteOrder/LITTLE_ENDIAN)
                   i)))

(defn double2bytes
  "Returns length nbytes Byte array representation of double f"
  [f nbytes]
  (.array (.putDouble (.order (ByteBuffer/allocate nbytes)
                           ByteOrder/LITTLE_ENDIAN)
                   f)))

(defn- bits
  "Returns sequence of s bits for an integer n, the bit order being from most
   to least significant. If the last bit is set, n is negative."
  [n s]
  (reverse (take s (map
                    (fn [i] (bit-and 0x01 i))
                    (iterate (fn [i] (bit-shift-right i 1))
                             n)))))

(defn- bits2byte
  "Turns a sequence of zeros and ones into a Byte"
  [bit-seq]
  (let [xpos (read-string (apply str (concat "2r" (rest bit-seq))))
        signum (first bit-seq)]
    (byte (if (= signum 1)
            (- xpos 128)
            xpos))))

(defn join-byte-arrays
  "Merges multiple Byte arrays"
  [& byte-arrays]
  (loop [bb (ByteBuffer/allocate (apply + (map count byte-arrays)))
         i 0]
    (if (< i (count byte-arrays))
      (recur (.put bb (nth byte-arrays i))
             (inc i))
      (.array bb) )))

(defn- get-bytes [barray]
  (map identity (barray)))

(defn get-bits
  "Returns a sequence of sequences of zeroes and ones representing the bits in the bytes of the array"
  [barray]
  (map #(bits % 8) barray))

(defn compress
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
        last-byte (bits2byte (if-let [lb (last degen-bytes)]
                               (reverse (conj (last degen-bytes) 0))
                               [0 0 0 0 0 0 0 0]))

        ]
    (byte-array (conj (vec first-bytes) last-byte))))
