(ns jekyll.bytecode
  (:import java.io.FileOutputStream))

(defn write-bytes-to-file [file byte-seq]
  (with-open [out (FileOutputStream. file)]
    (.write out (byte-array byte-seq))))

(defn vbri [i] ; works only for < 128 now
  (+ i 1))

(defn create-string [s]
   (concat [3 (.length s)] (.toCharArray s)))

(comment
  (write-bytes-to-file "/tmp/test"
                       (map byte (concat [(vbri 0)  ; register count
                                          (vbri 0)] ; instruction count
                                         (create-string "Hello World!")))))
