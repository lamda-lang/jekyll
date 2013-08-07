(ns jekyll.runtime)

(defmulti jeval first)

(defmethod jeval :NIL [val]
  nil)

(defmethod jeval :TRUE [val]
  true)

(defmethod jeval :FALSE [val]
  false)

(defmethod jeval :INTEGER [[_ & bytes]]
  (reduce #(+ (* 255 %1) %2) bytes))

#_(jeval [:INTEGER 0 0 0 0 0 0 1 42])

(defmethod jeval :FLOAT [[_ & bytes]]
  (java.lang.Double/longBitsToDouble (reduce #(+ (* 255 %1) %2) bytes)))

#_(jeval [:FLOAT 0 0 0 0 0 0 1 1])

(defmethod jeval :STRING [[_ len & chars]]
  (apply str chars))

#_(jeval [:STRING [:INTVLE32 [0 0 0 5]] "h" "e" "l" "l" "o"])

(defmethod jeval :LAMBDA [[_ arity count cont-ind-count code]]
  (eval `(fn ~(into [] (map #(symbol (str "a" %)) (range 1 (inc arity))))
           ~(jeval code))))

#_(jeval [:LAMBDA 5 1 -1 [:INTEGER 0 0 0 0 0 0 0 42]])

(defmethod jeval :RESULT [[_ lambda_index arg_count & arg_index-arg_count]]
  (apply (jeval lambda_index) (map jeval arg_index-arg_count)))

#_(jeval [:RESULT [:LAMBDA 1 1 -1 [:INTEGER 0 0 0 0 0 0 0 42]] 0])
