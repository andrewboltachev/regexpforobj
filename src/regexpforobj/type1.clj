(ns regexpforobj.type1
  (:require [clojure.core.typed :as t])
  )

(t/defalias T1
  (t/U
       (t/Seq t/Int)
       (t/Val :foo)
    )
  )

(t/ann f1 [T1 -> T1])
(defn f1 [x]
  (if
    (= x :foo)
    x
    (rest x)
    )
  )
