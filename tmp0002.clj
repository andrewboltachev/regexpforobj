
(defmacro grammar-symbol [name]
  `(defn ~name 
     ([~'value] (~name ~'value nil))
     ([~'value ~'payload] {:type ~(keyword name) :value ~'value :payload ~'payload})
     )
  )


