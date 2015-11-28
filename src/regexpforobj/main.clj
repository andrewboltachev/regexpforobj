(ns regexpforobj.main
  (:use regexpforobj.core)
  )

(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(def s1 (Seq [(Char "ref") (Star (Char "ref"))]))
(defn structure-conforms-to [pattern s]

  )

(defn main []
  (fipp
   #_(clojure.walk/prewalk (fn [x] (cond (and (map? x) (contains? x :type)) (assoc x :a 1) :else x)) s1)
    (structure-conforms-to
      (Seq ['x 'x])
      (Seq [(Char "ref") (Char "ref")]))
    )
  )
