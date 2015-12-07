(ns regexpforobj.core.typed
  ;(:use clojure.test)
  (:require [clojure.core.typed :as t])
  )

(defmacro grammar-symbol [name]
  `(defn ~name    
     ([~'value] (~name ~'value nil))
     ([~'value ~'payload] {:type ~(keyword name) :value ~'value :payload ~'payload})
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

#_(t/defalias SeqLike-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/Val :Char)
     }
    :complete? true
    )
  )

#_(t/defalias StarLike-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/U
                (t/Val :Char)
                (t/Val :InputChar)
                )
     }
    :complete? true
    )
  )

(t/defalias Grammar-type
  (t/Rec [x]
       (t/U
         Char-type
          (t/HMap
            :mandatory {:value (t/Vec x)
            :payload t/Any
            :type (t/U
                    (t/Val :Seq)
                    (t/Val :Or)
                    )
            }
            :complete? true
            )
          (t/HMap
            :mandatory {:value x
            :payload t/Any
            :type (t/U
                    (t/Val :MayBe)
                    (t/Val :Star)
                    )
            }
            :complete? true
            )
         )
       )
  )


(t/ann Char (t/IFn
                 [t/Any -> Char-type]
                 [t/Any t/Any -> Char-type]
                 ))
(grammar-symbol Char)

(t/ann Seq (t/IFn
                 [(t/Vec Grammar-type) -> Grammar-type]
                 [(t/Vec Grammar-type) t/Any -> Grammar-type]
                 ))
(grammar-symbol Seq)

(t/ann Or (t/IFn
                 [(t/Vec Grammar-type) -> Grammar-type]
                 [(t/Vec Grammar-type) t/Any -> Grammar-type]
                 ))
(grammar-symbol Or)

(t/ann Star (t/IFn
                 [Grammar-type -> Grammar-type]
                 [Grammar-type t/Any -> Grammar-type]
                 ))
(grammar-symbol Star)

(t/ann MayBe (t/IFn
                 [Grammar-type -> Grammar-type]
                 [Grammar-type t/Any -> Grammar-type]
                 ))
(grammar-symbol MayBe)

; results

(t/defalias InputChar-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/Val :InputChar)
     }
    :complete? true
    )
  )

(t/defalias InputStream-type
  (t/Vec
    InputChar-type
    )
  )

(t/defalias Input-type
  (t/Rec [x]
       (t/U
         InputChar-type
          (t/HMap
            :mandatory {:value (t/Vec x)
            :payload 
                        t/Any
                        ;(t/Val nil)
            :type 
                    (t/Val :SeqNode)
            }
            :complete? true
            )
         )
       )
  )
(t/ann InputChar (t/IFn
                 [t/Any -> InputChar-type]
                 [t/Any t/Any -> InputChar-type]
                 ))
(grammar-symbol InputChar)

(t/ann SeqNode (t/IFn
                 [(t/Vec Input-type) -> Input-type]
                 [(t/Vec Input-type) t/Any -> Input-type]
                 ))
(grammar-symbol SeqNode)



; =========================================



(t/defalias ParsingError-type
  (t/HMap
    :mandatory {
                 :error t/Keyword
                 :context t/Any
                 }
     :complete? true
    )
  )
; errors
(t/ann ParsingError (t/IFn
         [t/Keyword -> ParsingError-type]
         [t/Keyword t/Any -> ParsingError-type]
         ))
(defn ParsingError
  ([n] (ParsingError n nil))
  ([n context] {:error n :context context})
  )


(t/ann is_parsing_error? [t/Any -> t/Bool])
(defn is_parsing_error? [x]
  (or
    (and
      (map? x)
      (contains? x :error)
    )
    (nil? x)
    )
  )

(defn map2? [x]
  (map? x)
  )

(t/defalias ProcessFuncOutput-type
  (t/U
    (t/HVec [
             Input-type
             InputStream-type
             ])
    ParsingError-type
    )
  )

(t/ann process (t/IFn
                 [Grammar-type InputStream-type -> ProcessFuncOutput-type]
                 [Grammar-type InputStream-type t/Int -> ProcessFuncOutput-type]
                 ))
(defn process
  ([g x] (process g x 0))
  ([g x level]
  (let [level (or level 0)
        process1 (t/fn [g :- Grammar-type
                        x :- InputStream-type
                        ] :- ProcessFuncOutput-type (process g x (inc level)))
        ]
  ;(apply print (repeat level "\t"))
  ;(println "process" (grammar_pretty g) (vec (map grammar_pretty x)))
  (cond
    (= (:type g) :Char)
    (let [c (first x)] (if c
                         (let [vc (:value c)
                               vg (:value g)]
                           (if (= vg vc)
                             [c (vec (rest x))]
                             (ParsingError :char {:expected vg :found vc})
                             ))
                         (ParsingError :too-short-char {:rest g})
                         )
      )

    (= (:type g) :Seq)
    (t/loop [g1 :- (t/Vec Grammar-type), (:value g)
           x1 :- InputStream-type, x
           result :- (t/Vec Input-type), []
             ]
      (if (empty? g1)
        [(SeqNode result (:payload g)) x1]
        ;(if-not (empty? x1)
          (let [returned (process1 (first g1) x1)]
            (if
              (map2? returned)
              (assoc returned :a :b)
              (let [
                    new_v (first returned)
                    new_x (second returned)
                    ]
                (recur
                  (vec (rest g1))
                  new_x
                  (vec (conj result new_v))
                  )
                )
              )
            )
            ;(ParsingError :too-short-seq {:rest g1})
          ;)
        )
      )
    ;(SeqNode [(process1 (first (:value g)) x)])

    #_(= (:type g) :Or)
    #_(t/loop [g1 :- (t/Vec Grammar-type), (:value g)
           result :- (t/Vec ProcessFuncOutput-type), []]
      (if (empty? g1)
        (let [r 
                  (sort-by #(count (last %))
                           (filter #(not (is_parsing_error? %))
                                   result))
                         ]

          (if-not (empty? r)
            (let [rr (first r)]
              [(SeqNode (first rr) (:payload g)) (last rr)]
              )
            (ParsingError :or-fail)
          ))
        (let [rr (process1 (first g1) x)]
          (recur (rest g1) (conj result rr)))
        )
      )

    #_(= (:type g) :Star)
    #_(loop [g1 (:value g)
           x1 x
           result []]
      (let [r (process1 g1 x1)]
        (if (is_parsing_error? r)
          (do
            ;(apply print (repeat level "\t"))
            ;(println "star error1" r x1 (:payload g))
            [(SeqNode result (:payload g)) x1])
          (recur g1 (last r) (conj result (first r)))
          )
        )
      )

    #_(= (:type g) :MayBe)
    #_(let [r (process1 (:value g) x)]
      (if (is_parsing_error? r)
        [(SeqNode [] (:payload g)) x]
        [(SeqNode (first r) (:payload g)) (last r)]
        )
        )
  ))))

