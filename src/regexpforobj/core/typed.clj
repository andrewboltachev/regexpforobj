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
            :mandatory {:value (t/HVec [x])
            :payload t/Any
            :type (t/U
                    (t/Val :Seq)
                    (t/Val :Or)

                    (t/Val :SeqNode)
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

(grammar-symbol Char)
#_(defn f [x]
  (inc x)
  )

(grammar-symbol Char)
(grammar-symbol Seq)
(grammar-symbol Or)
(grammar-symbol Star)
(grammar-symbol MayBe)


; results
(grammar-symbol InputChar)
(grammar-symbol SeqNode)

; errors
(defn ParsingError
  ([n & [context]] {:error n :context context})
  )


(defn is_parsing_error? [x]
  (or
    (and
      (map? x)
      (contains? x :error)
    )
    (nil? x)
    )
  )

#_(defn process [g x & [level]]
  (let [level (or level 0)
        process (fn [g x] (process g x (inc level)))
        ]
  ;(apply print (repeat level "\t"))
  ;(println "process" (grammar_pretty g) (vec (map grammar_pretty x)))
  (cond
    (= (:type g) :Char)
    (let [c (first x)] (if c
                         (let [vc (:value c)
                               vg (:value g)]
                           (if (= vg vc)
                             [c (rest x)]
                             (ParsingError :char {:expected vg :found vc})
                             ))
                         (ParsingError :too-short-char {:rest g})
                         )
      )

    (= (:type g) :Seq)
    (loop [g1 (:value g)
           x1 x
           result []]
      (if (empty? g1)
        [(SeqNode result (:payload g)) x1]
        ;(if-not (empty? x1)
          (let [returned (process (first g1) x1)]
            (if (is_parsing_error? returned)
              returned
              (let [
                    [new_v new_x] returned
                    ]
                (recur (rest g1) new_x (conj result new_v))
                )
              )
            )
            ;(ParsingError :too-short-seq {:rest g1})
          ;)
        )
      )
    ;(SeqNode [(process (first (:value g)) x)])

    (= (:type g) :Or)
    (loop [g1 (:value g)
           result []]
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
        (let [rr (process (first g1) x)]
          (recur (rest g1) (conj result rr)))
        )
      )

    (= (:type g) :Star)
    (loop [g1 (:value g)
           x1 x
           result []]
      (let [r (process g1 x1)]
        (if (is_parsing_error? r)
          (do
            ;(apply print (repeat level "\t"))
            ;(println "star error1" r x1 (:payload g))
            [(SeqNode result (:payload g)) x1])
          (recur g1 (last r) (conj result (first r)))
          )
        )
      )

    (= (:type g) :MayBe)
    (let [r (process (:value g) x)]
      (if (is_parsing_error? r)
        [(SeqNode [] (:payload g)) x]
        [(SeqNode (first r) (:payload g)) (last r)]
        )
        )
  )))
