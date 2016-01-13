
(defn max (a b)
  (if (< a b) b a))

(defn fib (n)
  (if (< n 2)
    1
    (+ (fib (- n 2)) (fib (- n 1)))))

(defn blah (x s)
  (* x (strlen s)))

(defn fiz (x)
  (* 3.3 x))

(defn three (x)
  (if (= x 3)
    (println "Three!!!")
    (println "Not three...")))

(defn say ()
  (do (println "A")
      (println "B")
      (println "C")))

(defn eternal ()
  (while true
    (println "eternal")))

(register-builtin "fake" '() '(:ptr :int))
(register-builtin "fake2" '((:ptr :string)) :void)

(defn nullf ()
  (let [x (fake)]
    (null? x)))

