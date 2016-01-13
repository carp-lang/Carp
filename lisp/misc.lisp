(defn f-f ()
  123)

(defn g-g ()
  (f-f))

(defn conso2 (a b c)
  (do
    ;;(glVertex3f a b c) ;; 8
    ;;(glVertex3f b c a) ;; 40
    ;;(glVertex3f c a b) ;; 168
    ;;(glVertex3f c a b) ;; 680
    ;;(glVertex3f c a b) ;; 2728
    ;;(glVertex3f c a b) ;; 10920
    ))

(defn conso1 (x)
  (do
    (cosf x)
    (cosf x) ;; 12
    (cosf x) ;; 28
    (cosf x) ;; 60
    ;;(cosf x) ;; 124
    ))

(defn test-conso ()
  (do
    ;;(def conso-ast (form-to-ast '(cosf x))) 
    (def conso-ast (form-to-ast '(let [x 10f] (do (cosf x) (cosf x) (cosf x))))) ;;  (cosf x) (cosf x))))
    (def conso-con (generate-constraints conso-ast))
    (def conso-asta (annotate-ast conso-ast))
    ))

;;(test-conso)


;; (def v-ast (form-to-ast '(glVertex3f (+ 0.0f 0.8f) (+ 0.0f 0.7f) 0.0f)))
;; (def v-con (gencon v-ast))
;; (def v-asta (annotate-ast v-ast))

;; (defn vg (x)
;;   (glVertex3f x (+ 1.0f x) 2.0f))

;; (def vg-ast (lambda-to-ast (code vg)))
;; (def vg-con (gencon vg-ast))
;; (def vg-asta (annotate-ast v-ast))

