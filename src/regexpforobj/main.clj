(ns regexpforobj.main
  (:use regexpforobj.core)
  )

(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(def s1 (Seq [(Char "ref") (Star (Char "ref"))]))
(def c1 (Seq [(Char 'x) (Star (Char 'x))]))

(defn ifipp [x]
  (do
    (fipp x)
    x)
  )


(defn add-routes-to-grammar [s & [route]]
  (newline)
  (println "\t\tcalled" (grammar_pretty s))
  (ifipp (let [route (or route [])]
    (cond
      (and (map? s) (contains? s :type))
            (merge s {
                      :route route
                      :value

          (if
            (sequential? (:value s))
                     (into (empty (:value s))
                           (map-indexed
                             (fn [i v0]
                               (add-routes-to-grammar
                                 v0
                                 (conj route i)
                                 )
                               )
                             (:value s)
                             )
                           )

                  (add-routes-to-grammar
                    (:value s) (conj route :value)
                  )
                     )

                   })
      :else
      s
      )
        )
      )
  )

(defn structure-conforms-to2 [pattern s & [bindings]]
  ;...
  (let [bindings (or (bindings {}))
        v (get-in s (:route pattern))
        ]
    ; ...
    (cond
      (sequential? (:value pattern))
      (let [
            vm (into (empty (:value pattern)) 
              (map-indexed
                (fn [i v1]
                  (cond
                    (and (map? v1) (contains? v1 :type))
                    (structure-conforms-to v1 s bindings)

                    (symbol? v1)
                    (assoc bindings v1 (nth v))
                    

                    :else
                    (when (= (nth v) v1)
                      bindings
                      )
                    )
                  )
                 ))
            ]
        )

      (symbol? (:value pattern))
      (assoc bindings (:value pattern) v)

      :else
      (when
        (= (:value pattern) v)
        bindings
        )

      )
    )
  )

(defn structure-conforms-to [pattern s]
  ;...
  (let [pattern (add-routes-to-grammar pattern)]
    (structure-conforms-to2 pattern s)
    )
  )

#_(fn [x r]
                                      (if
                                        (and (map? x) (contains? x :type))
                                        (assoc x :route r)
                                        x
                                            )
                                      )
(comment
      ((Seq [(Char "ref") (Char "ref")]))
   #_(clojure.walk/prewalk (fn [x] (cond (and (map? x) (contains? x :type)) (assoc x :a 1) :else x)) s1)
    #_(structure-conforms-to
      (Seq ['x 'x])
  )
  )

(defn main []
  (
   ;fipp
   identity
    (let [c2 (add-routes-to-grammar c1)
          ]
      c2
      )
    )
  )
