;; These names are used for temp variables when emitting the C-code.

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
