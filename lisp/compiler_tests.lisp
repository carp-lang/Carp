
(defn test-constraint-solving-1 ()
  (let [constraints (list {:a :int :b "t0"}
                          {:a :string :b "t0"})]
    (do ;;(println "\nCon1:\n" (solve-constraints constraints))
        (println "\nCon1 reversed:\n" (solve-constraints (reverse constraints))))))

(defn test-constraint-solving-2 ()
  (let [constraints (list {:a :int :b "t0"}
                          {:a :int :b "t0"})]
    (do (println "\nCon1:\n" (solve-constraints constraints))
        (println "\nCon1 reversed:\n" (solve-constraints (reverse constraints))))))

(defn test-constraint-solving-3 ()
  (let [constraints (list {:a :int :b "t0"}
                          {:a :string :b "t1"})]
    (do (println "\nCon3:\n" (solve-constraints constraints))
        (println "\nCon3 reversed:\n" (solve-constraints (reverse constraints))))))

(defn test-constraint-solving-4 ()
  (let [constraints (list {:a "t0" :b "t0"}
                          {:a "t0" :b "t1"}
                          {:a "t1" :b "t1"})]
    (do (println "\nCon4:\n" (solve-constraints constraints))
        (println "\nCon4 reversed:\n" (solve-constraints (reverse constraints))))))

(defn test-constraint-solving-5 ()
  (let [constraints (list {:a "t0" :b "t0"}
                          {:a "t0" :b "t1"}
                          {:a "t1" :b :float}
                          {:a "t1" :b "t1"})]
    (do (println "\nCon5:\n" (solve-constraints constraints))
        (println "\nCon5 reversed:\n" (solve-constraints (reverse constraints))))))

;; (test-constraint-solving-2)
;; (test-constraint-solving-3)
;; (test-constraint-solving-4)
;; (test-constraint-solving-5)

(defn test-fib ()
  (do
    (def fibast (lambda-to-ast (code fib)))
    (def fibcon (gencon fibast))
    (def fibsolved (solve-constraints fibcon))
    (def fibasta (annotate-ast fibast))))

(defn foo (x)
  (+ (- x 100) 1))

(defn test-foo ()
  (do (bake foo)
      (assert-eq (foo 200) 101)
      (assert-eq (type foo) :foreign)
      :foo-is-ok))

;;(test-foo)

(defn hypo (x y)
  (sqrtf (+ (* x x) (* y y))))

(defn test-hypo ()
  (do (bake hypo)
      (assert-approx-eq (hypo 3.0 4.0) 5.0)
      (assert-eq (type hypo) :foreign)
      :hypo-is-ok))

;;(test-hypo)

(defn f (s)
  (strlen s))

(defn g (x s)
  (* x (f s)))

(defn h (x)
  (if "blorg" "Hej" x))

(def fast (lambda-to-ast (code f)))
(def fcon (gencon fast))
(def fasta (annotate-ast fast))

(def hast (lambda-to-ast (code h)))
(def hcon (gencon hast))
(def hasta (annotate-ast hast))

(defn fuck ()
  (+ "hej" 23))

(def fuckast (lambda-to-ast (code fuck)))
(def fuckcon (gencon fuckast))
;;(def fuckasta (annotate-ast fuckast))

(defn mix (x y z)
  (if (< (strlen z) 3) (* (itof y) x) x))

;; (def mixast (lambda-to-ast (code mix)))
;; (def mixcon (gencon mixast))
;; (def mixasta (annotate-ast mixast))

(defn monad ()
  (do (strlen "hej")
      (strlen "svej")
      (strlen "yay")))

(def monast (lambda-to-ast (code monad)))
(def moncon (gencon monast))
(def monasta (annotate-ast monast))



(defn test-loading ()
  (do
    (save "out/out.c" "int f() { return 100; }")
    (system "clang -shared -o out/f.so out/out.c")
    (def flib (load-dylib "out/f.so"))
    (register flib "f" () :int)
    (assert-eq 100 (f))
    
    (save "out/out.c" "int g() { return 150; }")
    (system "clang -shared -o out/g.so out/out.c")
    (def glib (load-dylib "out/g.so"))
    (register glib "g" () :int)
    (assert-eq 150 (g))
    
    (unload-dylib flib)
    
    (save "out/out.c" "int f() { return 200; }")
    (system "clang -shared -o out/f.so out/out.c")
    (def flib (load-dylib "out/f.so"))
    (register flib "f" () :int)
    (assert-eq 200 (f))
    ))

;; This does NOT work!
(defn shadow (x)
  (let [x (* x 3)]
    x))
;; (def shadowast (lambda-to-ast (code shadow)))
;; (def shadowcon (gencon shadowast))
;; (def shadowasta (annotate-ast shadowast))


(defn own1 ()
  (let [s "yeah"]
    (strlen s)))

(defn own2 ()
  (if true 10 20))

(defn own3 ()
  (while true (println "hello")))

(defn own4 (x)
  (+ (* 2 x) (+ 1 x)))

