(ns regexpforobj.main
  (:use
    regexpforobj.core
    regexpforobj.core.macros
    )
  )


;(grammar-symbol Plus)

(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(def s1 (Seq [(Char "ref") (Star (Char "ref"))]))
(def c1 (Seq [(Char 'x) (Star (Char 'x))]))
(def c2 (Seq ['x (Star 'x)]))
;(def r1 [(Seq ['x (Star 'x)]) (Plus 'x)])


(def s2 (Seq [
              ;(Char "ref")
              (Seq [(Char "ref") (Star (Char "ref"))])
              (Star
                ;(Char "ref")
                (Seq [(Char "ref") (Star (Char "ref"))])
                
                )]))

(def s3 (Seq [
              ;(Char "ref")
              ;(Seq [(Char "ref")])
              1
              
              (Star
                ;(Char "ref")
                ;(Seq [(Char "ref")])
                2
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
  ; TODO использовать метаданные для сохранения маршрутов веток дерева
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
                        (doall (map println ["map inside sequence"
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
                      (do
                        (println "this" v)
                      (assoc bindings v1 (nth (:value v) i))
                        )
                      

                      :else
                      (when (= (nth (:value v) i) v1)
                        bindings
                        )
                      )
                    )
                  (:value pattern)
                  ))
              _ (println "vm is")
              _ (fipp vm)
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
             (do

             (println "map inside map")
             (println "pattern")
             (println pattern)
             (println "v")
             (println v)
1
               )
             (=
              (:type pattern)
              (:type v)
              )
             
             )
        (structure-conforms-to2 (:value pattern) s bindings)

        (symbol? (:value pattern))
        (do
          #_(println "that's symbol, babe" pattern v)
          #_(println  "ahaha"
        (when
          (= (:type pattern) (:type v))
(assoc bindings (:value pattern) (:value v))
             
          ))
        (when
          (= (:type pattern) (:type v))
          (assoc bindings (:value pattern) (:value v))
             
          ))

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
   (fipp pattern)
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

(def x [(InputChar "x") (InputChar "x") (InputChar "L") (InputChar "x")])

(def g (let [x  (Char "x")] (Or [(Star x :star-1) (Star (Seq [(Char "L") (Star x :star-1)] :seq-1) :star-2)] :or-1))
  )


(defn
 apply-one-level
 [level-name g x]
  (println level-name \newline
           (grammar_pretty g)
           \newline
           (grammar_pretty x)
           \newline
           \newline
           \newline
           )
 (do (let [r (->
  (loop
   [[h & t :as l] x r [] b []]
   (if
    (some? h)
    (let
     [[rph rpr :as rp] (process g l) success (-> rph empty? not)
      
      ;_ (println rp)
      ]
     (if
      success
              (if-not (= rpr l)
      (concat b [(InputChar level-name rph)]
              (do
                ;(println "111" rp)
              (apply-one-level
                level-name
                g
                rpr)
                )
                )
                (if (empty? rpr) rpr
                  (cons (first rpr)
                          (apply-one-level
                            level-name
                            g
                            (rest rpr)
                          )
                  ))
              )
      (recur t (conj r rp) (conj b h))))
    x))
  vec)]
     (println r)
     (newline)

     r)
  
  ))


(defn symbol-to-c [gl]
  (clojure.walk/postwalk
    (fn [x]
      (if (symbol? x)
        (Char x)
        x
        )
      )
    gl
    )
  ) ;; TODO: you know what to do. this is just cheating so far


(defn run2 [g x]
  (let [glc (-> g my_fold symbol-to-c)]
   (loop [
          [[level-index rules :as h] & t] glc
          input x
          ]
     ; ...
     (if (some? h)
     (recur
       t
       (reduce
       (fn [current-input [i rule]]
          (apply-one-level
            (symbol (str "s" level-index "-" i))
            ; ...
            rule
            current-input
           )
         )
       input
       (map-indexed (fn [a b] [a b]) rules)
     )
       )
        
       input
       )
     )
   )
  )

(defn main []
  #_(
   fipp (grammar_pretty
   ;identity
    #_(let [c2 (add-routes-to-grammar c1)
          ]
      c2
      )
    #_(structure-conforms-to
      c2
      s2
      )
    #_(structure-conforms-to
      (Seq
                    ['x]
                    )

      (Seq [
                    (Star (Star (Char
                                  'x
                                  )))
                    ])
      )
   #_(apply-rule r1 (apply-rule r1 s2))

   #_(apply-rule r1 s2)
   ;(subvec glc 0 1)
          ;(run2 g x)
          #_(binding [*regexpforobj-debug1* true]
          (run (Seq [(Char "a") (Char "b") (Char "c")])
               [(InputChar "a") (InputChar "b") (InputChar "c")]
               )
            )
  ))

  #_(binding [*regexpforobj-debug1* true]
  (let [g (Plus (Char "a"))]
    (println (run g [(InputChar "a") (InputChar "a") (InputChar "a")]))
    (println (run g [(InputChar "a")]))
    (println (run g []))
    )
    )


  #_(binding [*regexpforobj-debug1* true]
  #_(fipp
    (grammar_pretty
      (run {
            :root1 (Or [:foo :bar])
            :root (Or [(Char (partial = "w"))
                        (Seq [
                              (Char "[")
                              :root
                              (Char "]")
                              ]
                          )
                        ])}
           :root1
            (map (comp InputChar str)
              (apply list "[[[[w]]]]"))
            ))))

  #_(println "this is RegExp")
  #_(binding [*regexpforobj-debug1* true]
  (fipp
    (grammar_pretty
      (run {
            :root (Or [(RegExp #"(w)(.*)")
                        (Seq [
                              (Char "[")
                              :root
                              (Char "]")
                              ]
                          )
                        ])}
           :root
            "[[[[w]]]]"))))

  #_(println
    (run (Plus (Char \w)) (map InputChar "w")))


  (binding [*regexpforobj-debug1* true]
    (let [pred
          (fn [p]
            (fn [v]
              (when (p v) v)
              )
            )
          gs 
         {:root (Star
                    (Or
                      [
                      (RawChar (pred integer?))
                      (GrammarChar
                           (Seq [
                                 (RawChar (pred (partial = :foo)))
                                 (RawChar (pred (partial = :bar)))
                                 ])
                           )
                       ]))
                  ;(RawChar (partial = :foo))
                  ;(RawChar (partial = :foo))
                  ;(Star (RawChar integer?))
          :root2
          (Star (Or [
               ; ...
               (RawChar (pred integer?))
               (RawChar (pred keyword?))
               (GrammarChar :root2)
               ]))
                  }]
      (binding [*print-meta* true]
    (println
      (;grammar_pretty
        identity
        (->>
          (run gs
             :root2
             [1 2 3 [:foo [1 2] :bar]]
             :meta-mark :mark1)
          ; ...
          (clojure.walk/postwalk
            (fn [value]
              (if-let [k (and (map? value) (:mark1 value))]
                ; ...
                (let [value1 (:value value)]
                  (if (true? k)
                    value1
                    {k value1}
                    ))
                value)))
          )))))))
