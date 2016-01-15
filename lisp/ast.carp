;; Anatomy of AST nodes
;; { :node = The kind of node this is. Can be :function / :arg / :literal / :app (function application) / :binop
;;   :type = The type that has been calculated for this node.
;;   :name = Used by AST nodes where this makes sense. The name of a variable or function, etc.
;; }

;; Anatomy of a type
;; Before the type is known it is set to a typevar which is a string of the form "t0", "t1", etc
;; Types can be just a keyword like :int / :string
;; Complex types are lists, like the :arrow type (:arrow (:int :int) :string) which corresponds to (Int, Int) -> String

(def typevar-counter 0)

(defn gen-typevar ()
  (let (typevar (str "t" typevar-counter))
    (do (swap! typevar-counter inc)
        typevar)))

(defn arg-list-to-ast (args)
  (map (fn (arg)
         {:node :arg
          :name arg
          :type (gen-typevar)})
       args))

(defn type-from-literal (lit)
  (match (type lit)
         :symbol (gen-typevar) ;; symbol means variable lookup, and we don't know the type of that variable
         x x))

(defn binop? (form)
  (match form
         (x & _) (contains? '(+ - * / <) x)
         _ false))

(defn gen-arrowtype (arg-count)
  (list :arrow (repeatedly gen-typevar arg-count) (gen-typevar)))

(defn list-to-ast (l)
  (if (binop? l)
    (match l
           (op a b) {:node :binop
                     :type (gen-typevar)
                     :op op
                     :a (form-to-ast a)
                     :b (form-to-ast b)})
    (match l
           ('= left right) {:node :binop
                            :type :bool
                            :op '==
                            :a (form-to-ast left)
                            :b (form-to-ast right)} 
           _ {:node :app
              :type (gen-typevar)
              :head (assoc (form-to-ast (first l)) :type (gen-arrowtype (count (rest l))))
              :tail (map form-to-ast (rest l))})))

(defn if-to-ast (expr a b)
  {:node :if
   :type (gen-typevar)
   :expr (form-to-ast expr)
   :a (form-to-ast a)
   :b (form-to-ast b)})

(defn do-to-ast (forms)
  {:node :do
   :type (gen-typevar)
   :forms (map form-to-ast forms)})

(defn bindings-to-ast (bindings)
  (match bindings
         (name value & rest-bindings) (cons {:node :binding
                                             :type (gen-typevar)
                                             :name name
                                             :value (form-to-ast value)}
                                            (bindings-to-ast rest-bindings))
         _ ()))

(defn let-to-ast (bindings body)
  {:node :let
   :type (gen-typevar)
   :bindings (bindings-to-ast bindings)
   :body (form-to-ast body)})

(defn while-to-ast (expr body)
  {:node :while
   :type :void
   :expr (form-to-ast expr)
   :body (form-to-ast body)})

(defn form-to-ast (form)
  (match form
         ('if expr a b) (if-to-ast expr a b)
         ('do & forms) (do-to-ast forms)
         ('let bindings body) (let-to-ast bindings body)
         ('while expr body) (while-to-ast expr body)
	 ('include-c-code s) {:node :c-code :code s :type (gen-typevar)}
         'NULL {:node :null :type (gen-typevar)}
         'true {:node :literal :type :bool :value 1}
         'false {:node :literal :type :bool :value 0}
         x (if (= :list (type x))
             (list-to-ast x)
             {:node :literal
              :type (type-from-literal x)
              :value x})))

(defn body-to-ast (body)
  (form-to-ast body))

;; Takes a list representation of a lambda and creates an AST from it
(defn lambda-to-ast (form)
  (do (assert-eq :list (type form))
      (match form
             ('fn args body) {:node :function
                              :type (gen-arrowtype (count args))
                              :args (arg-list-to-ast args)
                              :body (body-to-ast body)}
             _ :failed-to-match-lambda-form)))
