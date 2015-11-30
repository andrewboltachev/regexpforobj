(ns regexpforobj.core.typed
  (:use clojure.test)
  (:require [clojure.core.typed :as t])
  )


(defmacro grammar-symbol [name]
  `(defn ~name 
     ([~'value & [~'payload]] {:type ~(keyword name) :value ~'value :payload ~'payload})
     )
  )

(t/defalias Char-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/Val :Char)
     }
    :complete? true
    )
  )

(t/defalias SeqLike-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/Val :Char)
     }
    :complete? true
    )
  )

(grammar-symbol Char)
#_(defn f [x]
  (inc x)
  )
