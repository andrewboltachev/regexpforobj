(ns regexpforobj.core.macros)

; grammar
(defmacro grammar-symbol [name_]
  `(defn ~name_
     ([~'value & [~'payload]] {:type ~(keyword name_) :value ~'value :payload ~'payload})
     )
  )
