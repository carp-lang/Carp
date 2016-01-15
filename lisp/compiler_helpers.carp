(defn pretty-signature (t)
  (match t
         (:arrow args ret) (str "(" (join ", " (map pretty-signature args)) ") -> " (pretty-signature ret))
         x (if (keyword? t) (name t)
               (error (str "Invalid type signature: " t)))))
