(ns jekyll.bytecode
  (:import java.io.FileOutputStream))

(def opcodes [:C_STR :C_LAM])

(defn vbri [i] ; works only for < 128 now
  i)


(defn create-string [index s]
  (concat [(vbri 8) (vbri (.length s))] (.toCharArray s) [(vbri index)]))


(defn create-lambda [index arity code]
  (concat  [(vbri 4)                       ; opcode for lamda
            (vbri 1)                       ; register count for lambda TODO
            (vbri 1)                       ; instruction count TODO
            (vbri 0)                       ; context count TODO
            (vbri (count (map byte code))) ; code size
            (vbri arity)]
           code
           [(vbri index)]))


; add lambda for identity
(defn apply-lambda [target arity args result]
  (concat [(vbri 9) ; opcode for apply
           (vbri target)
           (vbri arity)]
           args
           [(vbri result)]))

; does not dispatch properly yet
(defmulti ->bytecode (comp keyword first))

(defmethod ->bytecode :fn [v] "is a fn")

(defmethod ->bytecode nil [v] (str v " is nil"))

#_(->bytecode ["fn"])

#_(->bytecode '(fn [a]))



(defn write-bytes-to-file [file byte-seq]
  (with-open [out (FileOutputStream. file)]
    (.write out (byte-array byte-seq))))

(comment
  (write-bytes-to-file "/tmp/test"
                       (map byte
                            (let [s (create-string 0 "Hello World!")
                                  l (create-lambda 1 0 s)
                                  a (apply-lambda 1 0 [] 0)]
                              (concat [(vbri 2)  ; register count
                                       (vbri 2)] ; instruction count
                                      l
                                      a)))))
; creates (2 2 4 1 1 0 15 0 8 12 \H \e \l \l \o \space \W \o \r \l \d \! 0 1 9 1 0 0)
