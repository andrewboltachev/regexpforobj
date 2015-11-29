(ns regexpforobj.main
  (:use regexpforobj.core)
  )


(grammar-symbol Plus)

(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(def s1 (Seq [(Char "ref") (Star (Char "ref"))]))
(def c1 (Seq [(Char 'x) (Star (Char 'x))]))
(def r1 [(Seq [(Char 'x) (Star (Char 'x))]) (Plus 'x)])


(def s2 (Seq [
              ;(Char "ref")
              (Seq [(Char "ref") (Star (Char "ref"))])
              (Star
                ;(Char "ref")
                (Seq [(Char "ref") (Star (Char "ref"))])
                
                )]))

(def s3 (Seq [
              (Char "ref")
              ;(Seq [(Char "ref")])
              ;1
              
              (Star
                (Char "ref")
                ;(Seq [(Char "ref")])
                ;1
                )]))

(defn ifipp [x]
  (do
    (fipp x)
    x)
  )


(defn add-routes-to-grammar [s & [route]]
  ;(newline)
  ;(println "\t\tcalled" (grammar_pretty s))
  (
   ;ifipp 
   identity
    (let [route (or route [])]
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
                                 (concat route [:value i])
                                 )
                               )
                             (:value s)
                             )
                           )

                  (add-routes-to-grammar
                    (:value s) (concat route [:value])
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
  (let [bindings (or bindings {})
        ;_ (println bindings)
        v (get-in s (:route pattern))
        ;_ (println (:route pattern) (grammar_pretty pattern) (grammar_pretty v))
        ]
    ; ...
    (if (and (= (:type v) (:type pattern))
             (= (:payload v) (:payload pattern)))
      (cond
        (sequential? (:value pattern))
        (let [
              vm (into (empty (:value pattern)) 
                (map-indexed
                  (fn [i v1]
                    (cond
                      (and (map? v1) (contains? v1 :type))
                      (do
                        #_(doall (map println ["map inside sequence"
                                (:route v1)
                                            (grammar_pretty v1)
                                            (grammar_pretty s)
                                            bindings
                                      ""
                                      ""
                                ]))

                      (structure-conforms-to2 v1 s bindings)
                                )

                      (symbol? v1)
                      (assoc bindings v1 (nth v))
                      

                      :else
                      (do
                        (println (nth v) v1)
                      (when (= (nth v) v1)
                        bindings
                        )
                        )
                      )
                    )
                  (:value pattern)
                  ))
              _ (println "vm is" vm)
              vm1 (apply merge-with
                (fn [va vb]
                  ;(println "comparing")
                  (when (= va vb)
                    va
                    )
                  )
                  vm
                )
              ;_ (println "vm1 is" vm1)
              ]
          (when (not-any? nil? (vals vm1))
            vm1
            )
          )

        (and (map? (:value pattern)) (contains? (:value pattern) :type)
             (=
              (:type (:value pattern))
              (:type v)
              )
             
             )
        (structure-conforms-to2 (:value pattern) s bindings)

        (symbol? (:value pattern))
        (assoc bindings (:value pattern) v)

        :else
        (do
          #_(println
            "hi"
            (:value pattern)
            v
            )
        (when
          (= (:value pattern) v)
          bindings
          )
          )

        )
      )
    )
  )

(defn structure-conforms-to [pattern s]
  ;...
  (let [pattern (add-routes-to-grammar pattern)]
    ;(fipp pattern)
    (structure-conforms-to2 pattern s)
    )
  )


(defn replace-bindings [bindings s]
  (clojure.walk/postwalk
    (fn [x]
      (if
        (and (symbol? x) (contains? bindings x))
        (bindings x)
        x
        )
      )
    s
    )
  )

(defn apply-rule1 [[from to] s]
  (if-let [bindings
    (structure-conforms-to from s)]
    (replace-bindings bindings to)
    s
    )
  )

(defn apply-rule2 [r s]
  (clojure.walk/postwalk
    (partial apply-rule1 r)
    s
    )
  )

(defn apply-rule [r s]
  (loop [s-old nil
         s s
         ]
    (if (= s-old s)
      s
      (recur s
        (apply-rule2 r s)
      ))
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
   fipp
   ;identity
    #_(let [c2 (add-routes-to-grammar c1)
          ]
      c2
      )
    ;(structure-conforms-to c1 s1)
   ;(apply-rule r1 (apply-rule r1 s2))

   (apply-rule1 r1 s3)
   )
  )
