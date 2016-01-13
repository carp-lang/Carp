;; Gotchas
;; * Unloading of function/dylib doesn't work after another function has linked to it during its compilation.
;; * Variable shadowing doesn't work properly when referencing itself

;; Anatomy of AST nodes
;; { :node = The kind of node this is. Can be :function / :arg / :literal / :app (function application) / :binop
;;   :type = The type that has been calculated for this node.
;;   :name = Used by AST nodes where this makes sense. The name of a variable or function, etc.
;; }

;; Anatomy of a type
;; Before the type is known it is set to a typevar which is a string of the form "t0", "t1", etc
;; Types can be just a keyword like :int / :string
;; Complex types are lists, like the :arrow type (:arrow (:int :int) :string) which corresponds to (Int, Int) -> String

;; How to add forms
;; 1. Make the ast generator generate a new kind of AST :node for the form (see above)
;; 2. The AST node should generate new type vars for all places where the type is unknown
;; 3. Make the constraint generator generate type constraints for the node
;; 4. Extend the function (assign-types) that substitute type variables for concrete types in the AST
;; 5. TODO: Make the borrow checker know about the node, if needed
;; 6. Make the C generator spit out beautiful C for the AST node
;; 7. Profit!

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



;;; Processing of the AST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The type env is bindings from variable names to types or variables, i.e. {:x :int, :y "t10"}
(defn type-env-extend (type-env args)
  (let [new-env (copy type-env)]
    (do (reduce (fn (_ pair) (dict-set! new-env (nth pair 0) (nth pair 1)))
                nil
                (map2 list (map :name args) (map :type args)))
        new-env)))

(defn is-self-recursive? (type-env app-f-name)
  (let [x (get-maybe type-env app-f-name)]
    (do
      ;;(println (str "app-f: " app-f-name ", x: " x))
      (= x :self))))

(defn get-type-of-symbol (type-env sym)
  (let [lookup (get-maybe type-env sym)]
    (if (= () lookup)
      (let [global-lookup (eval sym)]
        (type global-lookup))
      lookup)))

(defn math-op? (op)
  (contains? '(+ - * /) op))

(defn generate-constraints-internal (constraints ast type-env)
  (do
    ;;(println "gen constrs: \n" ast)
    (match (get ast :node)

           :function (let [extended-type-env (type-env-extend type-env (get ast :args))
                           extended-type-env-2 (let [fn-name (get-maybe ast :name)]
                                                 (if (string? fn-name)
                                                   (assoc extended-type-env fn-name :self)
                                                   extended-type-env))
                           new-constraints (generate-constraints-internal constraints (:body ast) extended-type-env-2)
                           func-ret-constr {:a (get-in ast '(:type 2)) ;; the return type of the arrow type
                                            :b (get-in ast '(:body :type))
                                            :doc (str "func-ret-constr")}
                           func-arg-constrs (map2 (fn (a b) {:a a :b b :doc "func-arg"})
                                                  (map :type (:args ast))
                                                  (get-in ast '(:type 1)))]
                       (concat func-arg-constrs (cons func-ret-constr new-constraints)))

           :app (let [ret-constr {:a  (get ast :type) :b (get-in ast '(:head :type 2)) :doc "ret-constr for :app"}
                      arg-constrs (map2 (fn (a b) {:a a :b b :doc "app-arg"}) (get-in ast '(:head :type 1)) (map :type (:tail ast)))
                      func-constrs (let [app-f-sym (get-in ast '(:head :value))
                                         app-f-name (str app-f-sym)
                                         app-f (eval app-f-sym)]
                                    (if (foreign? app-f)
                                      (list {:a (get-in ast '(:head :type)) :b (signature app-f) :doc "func-app"})
                                      (if (is-self-recursive? type-env app-f-name)
                                        () ;; no constraints needed when the function is calling itself
                                        (do (println (str "Calling non-baked function: " app-f-name " of type " (type app-f-sym) "\nWill bake it now!"))
                                            (bake-internal (new-builder) app-f-name (code (eval app-f-name)) '())
                                            (println (str "Baking done, will resume job."))
                                            (list {:a (get-in ast '(:head :type)) :b (signature (eval app-f-name)) :doc "freshly baked func-app"}))
                                      )))
                      tail-constrs (reduce (fn (constrs tail-form) (generate-constraints-internal constrs tail-form type-env))
                                           '() (:tail ast))
                      new-constraints (concat tail-constrs func-constrs (cons ret-constr arg-constrs))]
                  (concat new-constraints constraints))

           :literal (let [val (:value ast)]
                      (if (symbol? val) ;; if it's a symbol it's a lookup
                        (cons {:a (get ast :type)
                               :b (get-type-of-symbol type-env val)
                               :doc (str "lit-constr, lookup " val)} constraints)
                        constraints)) ;; other literals don't need constraints, just return unchanged constraints

           :binop (let [x0 (generate-constraints-internal constraints (get ast :a) type-env)
		        x1 (generate-constraints-internal x0 (get ast :b) type-env)
		        ;;tvar (gen-typevar)
                        ;;left-arg-constr {:a tvar :b (get-in ast '(:a :type)) :doc "left-arg-constr"}
                        ;;right-arg-constr {:a tvar :b (get-in ast '(:b :type)) :doc "right-arg-constr"}
			;;ret-constr {:a tvar :b (:type ast)}
                        same-arg-type-constr {:a (get-in ast '(:a :type)) :b (get-in ast '(:b :type)) :doc "same-arg-type-constr"}
                        maybe-constr (if (math-op? (:op ast))
                                       (list {:a (get-in ast '(:a :type)) :b (:type ast)})
                                       ())
			]
                    ;;(concat x1 (list left-arg-constr right-arg-constr ret-constr)))
                    (concat maybe-constr (cons same-arg-type-constr x1)))

           :if (let [x0 (generate-constraints-internal constraints (get ast :a) type-env)
                     x1 (generate-constraints-internal x0 (get ast :b) type-env)
                     x2 (generate-constraints-internal x1 (get ast :expr) type-env)
                     left-result-constr {:a (get-in ast '(:a :type)) :b (:type ast)}
                     right-result-constr {:a (get-in ast '(:b :type)) :b (:type ast)}
                     expr-must-be-bool {:a :bool :b (get-in ast '(:expr :type))}]
                 (concat x2 (list
                             expr-must-be-bool
                             left-result-constr
                             right-result-constr)))

           :do (let [x0 (reduce (fn (constrs form) (generate-constraints-internal constrs form type-env))
                                constraints (:forms ast))
                     ;;_ (log "count: " (count x0))
                     n (count (:forms ast))
                     ret-constr {:a (:type ast) :b (get-in ast (list :forms (- n 1) :type)) :doc "do-ret-constr"}]
                 (cons ret-constr x0))

           :let (let [bindings (:bindings ast)
                      extended-type-env (reduce (fn (e b) (assoc e (:name b) (get-in b '(:value :type)))) type-env bindings)
                      ;;_ (println "Extended type env: " extended-type-env)
                      let-constr {:a (:type ast) :b (get-in ast '(:body :type)) :doc "let-constr"}
                      bindings-constr (mapcat (fn (binding) (let [bind-constr {:a (:type binding) :b (get-in binding '(:value :type))}
                                                                  value-constrs (generate-constraints-internal constraints (:value binding) type-env)]
                                                              (cons bind-constr value-constrs)))
                                              bindings)
                      body-constrs (generate-constraints-internal constraints (:body ast) extended-type-env)]
                  (cons let-constr (concat bindings-constr body-constrs)))

           :while (let [x0 (generate-constraints-internal constraints (get ast :body) type-env)
                        x1 (generate-constraints-internal x0 (get ast :expr) type-env)
                        body-result-constr {:a (get-in ast '(:body :type)) :b (:type ast)}
                        expr-must-be-bool {:a :bool :b (get-in ast '(:expr :type))}]
                    (concat x1 (list expr-must-be-bool )))

           :null constraints
           
           _ constraints
           )))

(defn generate-constraints (ast)
  (let [constraints '()]
    (generate-constraints-internal constraints ast {})))

(def gencon generate-constraints)



(defn lookup (substs b)
  (let [val (get-maybe substs b)]
    (if (= () val)
      b
      (if (= b val)
          val
          (if (= :string (type val))
            (lookup substs val) ; keep looking
            val)) ; found the actual type
      )))


;; Replacement function for replacing "from the right" in an associative map2
;; Example usage:
;; (replace-subst-from-right {:a :b, :c :d} :d :e)
;; =>
;; {:c :e, 
;;  :a :b}

(defn maybe-replace-binding (key value replace-this with-this)
  (if (= replace-this value)
    {:key key :value with-this}
    {:key key :value value}))

(defn replace-subst-from-right (substs existing b)
  (reduce (fn (new-substs pair) (assoc new-substs (:key pair) (:value pair)))
          {}
          (map2 (fn (k v) (maybe-replace-binding k v existing b)) (keys substs) (values substs))))

(def log-substs false)

(defn typevar? (x) (string? x))

(defn extend-substitutions (substs a b)
  (do (when log-substs (println (str "\n" substs)))
      (when log-substs (println (str "\nEXTEND " a " " b)))
      (let [existing (get-maybe substs a)]
        (if (= () existing)
          (do (when log-substs (println (str "New substitution: " a " = " b)))
              (assoc substs a (lookup substs b)))
          (do (when log-substs (println (str "Found existing substitution for " a ", it was = " existing)))
              (let [replacement (lookup substs b)]
                (do
                  (when log-substs (println (str "Replacement: " replacement)))
                  (if (unify existing replacement)
                    (do (when log-substs (println "OK, replacement is the same."))
                        substs)
                    (if (typevar? replacement)
                      (if (typevar? (lookup substs a))
                        (do (when log-substs (println "Replace from right"))
                            (replace-subst-from-right substs existing replacement))
                        (do (when log-substs (println "Ignore this one"))
                            substs))
                      (if (typevar? existing)
                        (do (when log-substs (println "Replace existing typevar from right"))
                            (replace-subst-from-right substs existing replacement))
                        (error (str "Type checking failed, can't unify " replacement " with " existing))))))))))))

;; \nSubstitutions:\n" substs

(defn unify (a b)
  (if (and (list? a) (list? b))
    (all? true? (map2 unify a b))
    (if (= :any a)
      true
      (if (= :any b)
        true
        (= a b))))) ;; else clause

(defn solve-list (substs a-list b-list)
  (match (list a-list b-list)
         (() ()) substs
         ((a & as) (b & bs)) (solve (solve substs a b) as bs)
         _ (error (str "Lists not matching: " a-list " - vs - " b-list ", substs: \n" substs))))

(defn solve (substs a b)
  (if (and (list? a) (list? b))
    (solve-list substs a b)
    (if (string? a)
      (extend-substitutions substs a b)
      substs)))

(defn solve-contraint-internal (substs constraint)
  (let [a (:a constraint)
        b (:b constraint)]
    (solve (solve substs a b) b a))) ; Solving from both directions!

;; Returns a substitution map from type variables to actual types
(defn solve-constraints (constraints)
  (reduce solve-contraint-internal {} constraints))



(defn make-type-list (substs typevars)
  (map (fn (t) (if (string? t) (get-type substs t)
                   (if (list? t)
                     (make-type-list substs t)
                     t)))
       typevars))

(defn get-type (substs typevar)
  (if (list? typevar)
    (make-type-list substs typevar)
    (let [maybe-type (get-maybe substs typevar)]
      (if (= maybe-type ())
        typevar ;; lookup failed, there is no substitution for this type variable (= it's generic)
        maybe-type))))

(defn assign-types-to-list (asts substs)
  (map (fn (x) (assign-types x substs)) asts))

(defn assign-types-to-binding (b substs)
  (let [x0 (assoc b :type (get-type substs (:type b)))
        x1 (assoc x0 :value (assign-types (:value b) substs))]
    x1))

(defn assign-types (ast substs)
  (match (:node ast)
         :function (let [a (assoc ast :type (get-type substs (:type ast)))
                         b (assoc a :body (assign-types (:body ast) substs))
                         c (assoc b :args (assign-types-to-list (:args ast) substs))]
                     c)

         :app (let [app-ret-type (get-type substs (:type ast))]
                (assoc (assoc (assoc ast :type app-ret-type)
                              :head (assign-types (:head ast) substs))
                       :tail (map (fn (x) (assign-types x substs)) (:tail ast))))

         :literal (assoc ast :type (get-type substs (:type ast)))

         :arg (assoc ast :type (get-type substs (:type ast)))

         :binop (let [x0 (assoc ast :type (get-type substs (:type ast)))
                      x1 (assoc x0 :a (assign-types (:a ast) substs))
                      x2 (assoc x1 :b (assign-types (:b ast) substs))]
                  x2)

         :if (let [x0 (assoc ast :type (get-type substs (:type ast)))
                   x1 (assoc x0 :a (assign-types (:a ast) substs))
                   x2 (assoc x1 :b (assign-types (:b ast) substs))
                   x3 (assoc x2 :expr (assign-types (:expr ast) substs))]
               x3)

         :do (let [x0 (assoc ast :forms (map (fn (x) (assign-types x substs)) (:forms ast)))
                   x1 (assoc x0 :type (get-type substs (:type ast)))]
               x1)

         :let (let [x0 (assoc ast :bindings (map (fn (b) (assign-types-to-binding b substs)) (:bindings ast)))
                    x1 (assoc x0 :body (assign-types (:body x0) substs))
                    x2 (assoc x1 :type (get-type substs (:type ast)))]
                x2)
         
         :while (let [x0 (assoc ast :type (get-type substs (:type ast)))
                      x1 (assoc x0 :body (assign-types (:body ast) substs))
                      x2 (assoc x1 :expr (assign-types (:expr ast) substs))]
               x2)

         :null ast

	 :c-code (assoc ast :type (get-type substs (:type ast)))

         _ (error (str "Can't assign types to ast node " ast))))

;; x1 (assoc-in x0 '(:body :type) (get-type substs (get-in x0 '(:body :type))))


(defn get-deps (ast)
  (do
    ;;(println (str "visiting " ast))
    (if (dict? ast)
      (if (= :literal (:node ast))
        (if (symbol? (:value ast))
          (if (def? (:value ast))
            (if (ffi? (eval (:value ast)))
              '()
              (do (println (eval (:value ast))) (list (:value ast))))
            (do (println (str (:value ast) " " (type (:value ast)) " not defined")) (list (:value ast))))
          '())
        (mapcat get-deps (values ast)))
      (if (list? ast)
        (mapcat get-deps ast)
        '()))))

(def name-counter 0)

(defn gen-name ()
  (do (swap! name-counter inc)
      (str "" name-counter)))

(defn generate-names (ast)
  (do
    ;;(println (str "genname for " (:node ast)))
    (match (:node ast)

           :function (let [ast1 (assoc ast :body (generate-names (:body ast)))]
                       ast1)
           
           :if (let [if-result-name (str "if_result_" (gen-name))
                     if-expr-name (str "if_expr_" (gen-name))
                     ast1 (assoc ast :if-result-name if-result-name)
                     ast2 (assoc ast1 :if-expr-name if-expr-name)
                     ast3 (assoc ast2 :a (generate-names (:a ast)))
                     ast4 (assoc ast3 :b (generate-names (:b ast)))]
                 ast4)

           :while (let [while-expr-name (str "while_expr_" (gen-name))
                        ast1 (assoc ast :while-expr-name while-expr-name)
                        ast2 (assoc ast1 :body (generate-names (:body ast)))]
                    ast2)
           
           :let (let [let-result-name (str "let_result_" (gen-name))
                      ast1 (assoc ast :bindings (map generate-names (:bindings ast)))
                      ast2 (assoc ast :body (generate-names (:body ast)))
                      ast3 (assoc ast2 :let-result-name let-result-name)]
                  ast3)
           
           :binding ast ;; (let [] (log "binding:\n" ast))
           
           :app (let [head (:head ast)
                      func-name (:value head)
                      c-func-name (c-ify-name (str func-name))
                      app-result-name (str c-func-name "_result_" (gen-name))

                      args (:tail ast)
                      arg-names (repeatedly (fn () (str "arg_" (gen-name))) (count args))
                      
                      ast1 (assoc ast :head (generate-names (:head ast)))
                      ast2 (assoc ast1 :tail (map (fn (node) (generate-names node)) (:tail ast1)))
                      ast3 (assoc ast2 :app-result-name app-result-name)
                      
                      ast4 (assoc ast3 :tail (map2 (fn (arg arg-name) (assoc arg :arg-name arg-name)) (:tail ast3) arg-names))
                      ]
                  ast4)

           :binop (let [ast1 (assoc ast :a (generate-names (:a ast)))
                        ast2 (assoc ast1 :b (generate-names (:b ast)))]
                    ast2)
                  
           :do ast
           
           :literal ast
           
           _ (error (str "Can't generate name for node " ast))
           
           )))


(defn calculate-lifetimes (ast)
  ast)


(defn annotate-ast (ast)
  (let [constraints (generate-constraints ast)
        substs (solve-constraints constraints)
        ast-typed (assign-types ast substs)
        ast-names (generate-names ast-typed)
        ast-lifetimes (calculate-lifetimes ast-names)]
    ast-lifetimes))


;;; Code gen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn c-ify-name (lisp-name)
  (let [x0 (str-replace lisp-name "-" "_")
        x1 (str-replace x0 "?" "QMARK")
        x2 (str-replace x1 "!" "BANG")]
    x2))

(defn type-build (t)
  (if (string? t)
    "typevar"
    (match t
           :? "unknown"
           (:arrow _ _) "arrow"
           (:ptr p) (str (name p) "*")
           x (name x))))

;; Creates a C code builder which allows for out-of-order generation of C from the AST
(defn new-builder ()
  {:headers ()
   :functions ()})

(defn builder-add (builder category block)
  (update-in builder (list category) (fn (blocks) (cons-last blocks block))))

;; Takes a completed C code builder and returns its string with C code
(defn builder-merge-to-c (builder)
  (let [funcs (get builder :functions)
        headers (get builder :headers)]
    (join "\n\n"
          (list (join "\n" headers)
                (join "\n" funcs)))))

(def indent-level 1)

(defn indent ()
  (join "" (replicate "  " indent-level)))

(defn indent-in! ()
  (swap! indent-level inc))

(defn indent-out! ()
  (swap! indent-level dec))

(defn free-variables! (c free-list)
  (do
    (str-append! c (str (indent) "/* FREE: */\n"))
    (map (fn (variable)
           (str-append! c (str (indent) "free(" variable ")")))
         free-list)
    (str-append! c (str (indent) "/* * * * */\n"))))

(defn visit-arg (c arg)
  (let [result (visit-form c arg true)]
    (str-append! c (str (indent) (type-build (:type arg)) " " (:arg-name arg) " = " (get result :c) ";\n"))))

(defn visit-args (c args)
  (let []
    (do
      ;;(println "visit args:" args)
      (map (fn (arg) (visit-arg c arg)) args)
      (map (fn (arg) {:c (:arg-name arg)}) args))))

(defn visit-bindings (c bindings)
  ;;(println bindings)
  (map (fn (b) (let [value-result (visit-form c (:value b) false)]
                 (str-append! c (str (indent) (type-build (:type b)) " " (:name b) " = " (:c value-result) ";\n"))))
       bindings))

(defn visit-form (c form toplevel)
  (match (get form :node)

         :binop (let [result-a (visit-form c (get form :a) false)
                      result-b (visit-form c (get form :b) false)]
                  {:c (str (if toplevel "" "(") (:c result-a) " " (:op form) " " (:c result-b) (if toplevel "" ")"))})
         
         :literal (let [val (:value form)]
                    (if (symbol? val)
                      {:c (c-ify-name (name val))}
                      (if (string? val)
                        {:c (str "strdup(" (prn val) ")")}
                        {:c (prn val)})))

         :if (let [if-expr (visit-form c (get form :expr) true)
                   n (get form :if-result-name)
                   ifexpr (get form :if-expr-name)]
               (do (str-append! c (str (indent) (type-build (get-in form '(:expr :type))) " " ifexpr " = " (get if-expr :c) ";\n"))
                   (if (= :void (:type form))
                     () ;; no result variable needed
                     (str-append! c (str (indent) (type-build (:type form)) " " n ";\n")))
                   
                   (str-append! c (str (indent) "if(" ifexpr ")"))
                                
                   ;; true-block begins
                   (str-append! c " {\n")
                   (indent-in!)
                   (let [result-a (visit-form c (get form :a) true)]
                     (do
                       (if (= :void (:type form))
                         () ;; no-op
                         (str-append! c (str (indent) n " = " (get result-a :c) ";\n")))
                       (indent-out!)
                       (str-append! c (str (indent) "} else {\n"))))
                   
                   (indent-in!) ;; false-block-begins
                   (let [result-b (visit-form c (get form :b) true)]
                     (do
                       (if (= :void (:type form))
                         () ;; no-op
                         (str-append! c (str (indent) n " = " (get result-b :c) ";\n")))
                       (indent-out!)
                       (str-append! c (str (indent) "}\n"))))
                   {:c n}))
  
         :app (let [head (get form :head)
                    func-name (get head :value)
                    c-func-name (c-ify-name (str func-name))
                    n (:app-result-name form)
                    arg-results (visit-args c (get form :tail))
                    arg-vars (map (fn (x) (get x :c)) arg-results)]
                (do (if (= :void (:type form))
                      (str-append! c (str (indent) c-func-name "(" (join ", " arg-vars) ");\n"))
                      (str-append! c (str (indent) (type-build (:type form)) " " n " = " c-func-name "(" (join ", " arg-vars) ");\n")))
                    {:c n}))

         :do (let [forms (:forms form)
                   results (map (fn (x) (visit-form c x toplevel)) forms)]
               {:c (:c (last results))})

         :let (let [n (:let-result-name form)]
                (do (if (= :void (:type form))
                      () ;; nothing
                      (str-append! c (str (indent) (type-build (:type form)) " " n ";\n")))
                    (str-append! c (str (indent) "{\n"))
                    (indent-in!)
                    (let [body (:body form)
                          _ (visit-bindings c (:bindings form))
                          result (visit-form c body false)]
                      (do (if (= :void (:type form))
                            ()
                            (str-append! c (str (indent) n " = " (:c result) ";\n")))
                          ;;(free-variables! c )
                          ))
                    (indent-out!)
                    (str-append! c  (str (indent) "}\n"))
                    {:c n}))

         :while (let [while-expr (visit-form c (get form :expr) true)
                      while-expr-name (:while-expr-name form)]
                  (do (str-append! c (str (indent) (type-build (get-in form '(:expr :type))) " " while-expr-name " = " (get while-expr :c) ";\n"))
                      (str-append! c (str (indent) "while(" while-expr-name ") {\n"))
                      (indent-in!)
                      (let [body (:body form)]
                        (visit-form c body false))
                      (let [while-expr-again (visit-form c (get form :expr) true)]
                        (str-append! c (str (indent) while-expr-name " = " (get while-expr-again :c) ";\n")))
                      (indent-out!)
                      (str-append! c  (str (indent) "}\n"))))

	 :c-code (do
		     ;;(str-append! c )
		     {:c (:code form)})

         :null {:c "NULL"}
  
         x (error (str "visit-form failed to match " x))))

(defn arg-list-build (args)
  (join ", " (map (fn (arg) (str (type-build (get arg :type)) " " (get arg :name)))args)))

(defn visit-function (builder ast func-name)
  (let [t (:type ast)
        _ (when (not (list? t)) (error "Can't generate code for function, it's type is not a list."))
        return-type (nth t 2)
        args (get ast :args)
        body (get ast :body)
        c (copy "") ;; mutable string holding the resulting C code for the function
        result (visit-form c body true)
        ]
    (do
      ;;(println "visit-function: \n" ast)
      (let [code (str (name return-type) " " (c-ify-name func-name)
                      "(" (arg-list-build args) ") {\n"
                      c 
                      (if (= :void (:type body))
                        "" ;; no return
                        (str (indent) "return " (get result :c) ";\n"))
                      "}")]
        (builder-add builder :functions code)))))

(defn get-function-prototype (ast func-name)
  (let [t (get ast :type)
        return-type (nth t 2)
        args (get ast :args)]
    (str (name return-type) " " func-name "(" (arg-list-build args) ");")))

(defn ast-build (builder ast func-name)
  (match (get ast :node)
         :function (visit-function builder ast func-name)
         x (error (str "Can't match :ast '" x "' in ast-to-c."))))

(def files (list "\"functions.h\"" "\"shared.h\""))

(defn add-headers (builder)
  (reduce (fn (b file) (builder-add b :headers (str "#include " file)))
          builder
          files))

(def funcs {})

(defn add-func! (func-name func-proto func-dylib)
  (swap! funcs (fn (fs) (assoc fs func-name {:func-name func-name
                                             :func-proto func-proto
                                             :func-dylib func-dylib}))))

(defn unload-if-necessary (func-name)
  (map (fn (f)
         (when (= func-name (get f :func-name))
           (let [dylib (get f :func-dylib)]
             (do (println (str "Unloading dylib " dylib " for function " func-name "."))
                 (unload-dylib dylib)
                 (dict-remove! funcs func-name)))))
       (values funcs)))

(defn unload-all ()
  (join "\n" (map (fn (x) (str (unload-dylib (:func-dylib x)))) (values funcs))))

(defn save-prototypes ()
  (save "out/functions.h"
        (str
         "#include \"shared.h\"\n"
         (join "\n" (map c-ify-name (map :func-proto (values funcs)))))))

(defn link (ignore-this)
  (join " " (map (fn (f) (str "./out/" f ".so"))
                 (filter (fn (x) (not (= x ignore-this))) (map :func-name (values funcs))))))

(defn link-libs (dependencies)
  (join " " (map (fn (f) (str "./out/" (c-ify-name (str f)) ".so")) dependencies)))

(defn include-paths ()
  "-I/usr/local/include")

(defn lib-paths ()
  "-L/usr/local/lib/ -lglfw3")

(defn framework-paths ()
  "-framework OpenGL -framework Cocoa -framework IOKit")

(defn pretty-signature (t)
  (match t
         (:arrow args ret) (str "(" (join ", " (map pretty-signature args)) ") -> " (pretty-signature ret))
         x (if (keyword? t) (name t)
               (error (str "Invalid type signature: " t)))))

(def compile-exe false)

;; Takes a function name and the list representation of the lambda
(defn bake-internal (builder func-name func-code dependencies)
  (let [ast (lambda-to-ast func-code)
        ast-named (assoc ast :name func-name)
        ast-annotated (annotate-ast ast-named)
        builder-with-headers (add-headers builder)
        new-builder (ast-build builder-with-headers ast-annotated func-name)
        c-program-string (builder-merge-to-c new-builder)
        proto (get-function-prototype ast-annotated func-name)
        c-file-name (str "out/" func-name ".c")
        c-func-name (c-ify-name func-name)]
    (do
      (def ast ast-annotated)
      (def c c-program-string)
      (match (get ast-annotated :type)
             (:arrow arg-types return-type)
             (do
               (save-prototypes)
               (save c-file-name c-program-string)
               (let [clang-command (str "clang "
                                        (if compile-exe
                                          (str "-o out/a.out ")
                                          (str "-shared -o out/" c-func-name ".so "))
                                        c-file-name " "
                                        (include-paths)  " "
                                        (lib-paths) " "
					(framework-paths) " "
                                        (link-libs dependencies))]
                 (do
                   (println clang-command)
                   (def cmd clang-command)
                   (system clang-command)))
               (unload-if-necessary func-name)
               (def out-lib (load-dylib (str "./out/" c-func-name ".so")))
               (register out-lib c-func-name arg-types return-type)
               (add-func! func-name proto out-lib)
	       (let [f (eval (read func-name))]
                 (do (def s (pretty-signature (signature f)))
                     f)))
             _ (error "Must bake function with type (:arrow ...)")))))

;; Bake a function in the current environment, just give it's symbol
(defmacro bake (func-symbol)
  (list 'bake-internal (new-builder) (str func-symbol) (list 'code func-symbol) '()))

(defmacro bake* (func-symbol dependencies)
  (list 'bake-internal (new-builder) (str func-symbol) (list 'code func-symbol) dependencies))

(defn run-compiler-tests () (load "compiler_tests.lisp"))

(load (str carp-dir "lisp/compiler_tests.lisp"))

