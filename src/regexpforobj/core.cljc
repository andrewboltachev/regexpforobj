(ns regexpforobj.core
  #?(
     :cljs
     (:require 
                [clojure.string]
                [clojure.walk]
                )
     :clj
     (:require
       [regexpforobj.core.macros :refer [grammar-symbol]]
       )
     )
  #?(
     :cljs
     (:require-macros [regexpforobj.core.macros :refer [grammar-symbol]])

     :clj
      (:use clojure.test)
      )
  )


#?(:clj
     (require
      '[io.aviso.ansi :as font]
       )
    )
#?(:clj
      (use 'aprint.core)
    )

#?(:cljs
    (enable-console-print!))


(def ^:dynamic *regexpforobj-debug1* false)

(def process-calls (atom nil))

(grammar-symbol Char)
(grammar-symbol RegExp)
(grammar-symbol Seq)
(grammar-symbol Or)
(grammar-symbol Star)
(grammar-symbol Plus)
(grammar-symbol MayBe)
(grammar-symbol RawChar)
(grammar-symbol GrammarChar)


; results
(grammar-symbol InputChar)
(grammar-symbol SeqNode)


; errors
(defn ParsingError
  ([name_ & [context]] {:error name_ :context context})
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

(defn grammar_pretty [m] (cond
               (and
                 (map? m)
                 (contains? m :type)
                 )
               (let [key-repr (if (keyword? (:type m))
                           (symbol (name (:type m)))
                           (:type m) ;; fallback, whatever it is
                           )
                     
                     r (if (nil? (:value m))
                             key-repr
                             (list key-repr
                         (grammar_pretty (:value m))))
                     p (:payload m)]
                 (if
                   (nil? p)
                   r
                   (concat r [p])
                   )
                 )
               
               (sequential? m)
               (vec (map grammar_pretty m))

               :else
               m
               ))

(defn run-p
  [process-fn]
  (fn
  [& args]
   (let [[gs g x & {:keys [] :as options}]
   (if (= (count args) 2)
     [{:root (first args)} :root (second args)]
     args)]
   (reset! process-calls 0)
    (let [returned (apply process-fn gs g x 1 nil (flatten (seq options)))]
      (if (is_parsing_error? returned)
        returned
        (let [[result tail] returned]
          (if (= (count tail) 0)
            result
            (ParsingError :tail {:tail tail}))))))))

(defn process-Y [process-fn]
  (fn [gs g x level data-fn & {:keys [meta-mark] :as options}]
  (swap! process-calls inc)
  (let [
g-original g
        SeqNode (fn [& args]
                  (vary-meta (apply SeqNode args) merge (meta g))
                  )
        colored true
        level (or level 0)
        level-ident (apply str (repeat (* (inc level) 4) " "))
        print-with-level-ident (fn [color lines]
                    (identity (doall (map println
                                                   (filter #(-> % empty? not)
                                                      (map (fn [line] (str (when colored color)
                                                                   (inc level)
                                                                   level-ident
                                                                   (clojure.string/trim line)
                                                                   #?(:clj (when colored font/reset-font))
                                                                   ))
                                                   (clojure.string/split-lines lines)
                                                   )))))
                    )

        g (if
            (keyword? g)
            (let [mapped-g (gs g)]

        #?(:clj (when *regexpforobj-debug1*
              (print-with-level-ident font/yellow-font (str "mapping g " (pr-str g))))
                :cljs nil)
              mapped-g)
              g)
        _ #?(:clj (when *regexpforobj-debug1*
                             (print-with-level-ident font/green-font 
                               (pr-str (grammar_pretty g)
                               ))
                             (print-with-level-ident font/blue-font 
                               (pr-str (grammar_pretty x)
                               ))
                      )
                  :cljs nil
                  )
        ]

    (if g
(let [incorrect_alias_detected #(and (is_parsing_error? %) (-> % :error (= :no-such-alias)))
      result 
  (let [process (fn [g x] (apply
                            process-fn gs g x (inc level) data-fn
                            (flatten (seq options))))

        SeqNode
        (if
          (some? meta-mark)
          (fn [& args]
            (merge (apply SeqNode args)
                   {meta-mark (if (keyword? g-original)
                                   g-original true)}))
          SeqNode
          )

        ]
  ;(apply print (repeat level "\t"))
  ;(println "process" (grammar_pretty g) (vec (map grammar_pretty x)))
  (cond
    (not (sequential? x))
    ;(process g [x])
    (ParsingError
      :not-sequential-argument
      {:x x
       :g g})

    (= (:type g) :Char)
    (let [c (first x)] (if c
                         (let [vc (:value c)
                               vg (:value g)
                               
                               condition (if (fn? vg)
                                           (vg vc)
                                           (= vg vc))]
                           (if condition
                             [c (rest x)]
                             (ParsingError :char {:expected vg :found vc})
                             ))
                         (ParsingError :too-short-char {:rest g})
                         )
      )

    (= (:type g) :RawChar)
    (let [c (first x)] (if c
                         (let [vg (:value g)
                               condition (vg c)]

                           (if condition
                             (if (is_parsing_error? condition) ; TODO: may be intersection???
                               condition
                               [condition (rest x)])
                             (ParsingError :char {:expected vg :found c})
                             ))
                         (ParsingError :too-short-char {:rest g})
                         )
      )

    (= (:type g) :GrammarChar)
    (process
      (RawChar
        #(apply
           (run-p process-fn) gs (:value g)
           %
           (flatten (seq options))
           )) x)

    (= (:type g) :RegExp)
    (if-let [a (re-find (:value g) x)]
      [a (subs x (count a))]
      (ParsingError :regexp-mismatch {:expected (:value g) :found x}))

    (= (:type g) :Seq)
    (loop [g1 (:value g)
           x1 x
           result []]
      (if (empty? g1)
        [(SeqNode result (:payload g)) x1]
        ;(if-not (empty? x1)
          (let [returned (process (first g1) x1)]
            (if (incorrect_alias_detected returned)
              returned
              (if (is_parsing_error? returned)
                returned
                (let [
                    [new_v new_x] returned
                    ]
                (recur (rest g1) new_x (conj result new_v))
                )
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

          (if-let [e (first (filter incorrect_alias_detected result))]
            e
          (if-not (empty? r)
            (let [rr (first r)]
              [(SeqNode (first rr) (:payload g)) (last rr)]
              )
            (ParsingError :or-fail {:result result
                                    :g g-original})
          )))
        (let [rr (process (first g1) x)]
          (recur (rest g1) (conj result rr)))
        )
      )

    (= (:type g) :Plus)
    (let [r (process (:value g) x)]
      (if (is_parsing_error? r)
        r
        (let [x (last r)
              a (first r)
              ]
          (if
            (empty? x)
            [(SeqNode (cons a []) (:payload g)) x]
            (loop [g1 (:value g)
                  x1 x
                  result []
                  ]
              (let [r (process g1 x1)]
                (if (or (empty? x1) (is_parsing_error? r) (= x1 (last r)))
                  (if (incorrect_alias_detected r)
                    r
                  (do
                    ;(apply print (repeat level "\t"))
                    ;(println "star error1" r x1 (:payload g))
                    [(SeqNode (cons a (vec result)) (:payload g)) x1]))
                  (recur g1 (last r) (conj result (first r)))
                  )
                )
              )
            )
          )
        ) 
      )

    (= (:type g) :Star)
    (if
      (empty? x)
      [(SeqNode [] (:payload g)) x]
      (loop [g1 (:value g)
            x1 x
            result []
             ]
        (let [r (process g1 x1)]
          (if (or (empty? x1) (is_parsing_error? r) (= x1 (last r)))
            (if (incorrect_alias_detected r)
              r
            (do
              ;(apply print (repeat level "\t"))
              ;(println "star error1" r x1 (:payload g))
              [(SeqNode result (:payload g)) x1]))
            (recur g1 (last r) (conj result (first r)))
            )
          )
        )
      )

    (= (:type g) :MayBe)
    (let [r (process (:value g) x)]
      (if (incorrect_alias_detected r)
        r
      (if (is_parsing_error? r)
        [(SeqNode [] (:payload g)) x]
        [(SeqNode (first r) (:payload g)) (last r)]
        )
        ))
  ))
      ]

#?(:clj (when *regexpforobj-debug1*
                             (print-with-level-ident font/red-font 
                               (pr-str (grammar_pretty result))
                               )
  ))
result
)
{:error :no-such-alias
 :context g-original}
)

)))

(defn Y [f] ((fn [x] (x x)) (fn [x] (f (fn [& args] (apply (x x) args))))))

(def process (Y process-Y))

(def run (run-p process))

;(defmacro mymacro1 [a] `(defn ~a [x11] (+ 1 x11)))

(defn positions [x coll]
  (flatten (map-indexed #(if (= %2 x) [%1] []) coll)))

(defn
  tree-depth
  [branch? children count-empty? root]
  (let
   [walk
    (fn
      walk
      [node]
      (if
       (branch? node)
        (let
         [children (children node)]
          (if
           (empty? children)
            (if count-empty? 1 0) 
            (inc (reduce max (map walk children)))))
        0))]
    (walk root)))



(defn get_pos [x variants]
  (first (positions x variants)))

(defn replace_subst [first_one data]
  (map
   (fn [[depth item]]
     [depth (clojure.walk/postwalk
             (fn [x]
               (let [pos (get_pos x (last first_one))]
                 (if (nil? pos)
                   x
                   (symbol (str "s" (first first_one) "-" pos))
                   )))
             item)])
   data))

(defn nodes_for_my_fold [nodes]
  ;(println (class x) (coll? x))
  (let [branch? (fn [x] (map? x)) ; coll?
        children (fn [x] (let [v (:value x)] (if (and (coll? v) (not (map? v))) v [v]))) ;(fn [c] ((if (map? c) :value seq) c))
        tree-depth-func #(tree-depth branch? children true %)
        max_depth (tree-depth-func nodes)
        attach-depth #(map (fn [subtree] {:el subtree :depth (tree-depth-func subtree)}) %)]
        (let [this-seq (tree-seq branch? children nodes)
              depth-attached (attach-depth (distinct this-seq))
              freqs (frequencies this-seq)
              freq-for #(get freqs %)
              attach-freq #(map (fn [x] (assoc x :freq (freq-for (:el x)))) %)

              stuff-attached (attach-freq depth-attached)
              filtered (filter #(or (and (> (:freq %) 1) (> (:depth %) 1)) (= (:depth %) max_depth)) stuff-attached)]
          (sort-by first (map (fn [[d els]] [d (map :el els)]) (seq (group-by :depth filtered)))))
    )
  )

(defn my_fold_nodes [nodes]
    (loop [nodes nodes
           result []]
      (if-not (empty? nodes)
        (do
          (let [first_one (first nodes)]
            (recur
             (let [x00
                   (replace_subst first_one (rest nodes))]
                ;(println (first first_one))
                ;(println (class x00))
                ;(println)
               x00
                ;(rest nodes)
               )
             (conj result first_one))))
        result)))


(defn my_fold [nodes]
  (my_fold_nodes (nodes_for_my_fold nodes))
  )

(defn make_let [data]
  (let [w (mapcat (fn [[d v]] (map-indexed #(do [(symbol (str "s" d "-" %1)) %2]) v)) data)
        s (list 'let (vec (mapcat identity w)))]
    (concat s [(first (last w))])))


(defn ^:private Y [f] ((fn [x] (x x)) (fn [x] (f (fn [& args] (apply (x x) args))))))


(def process1 (fn [r]
           (clojure.walk/postwalk
             (fn [x]
               (if-let [f (-> x meta :process1)]
                 (do
                   (vary-meta (f x) dissoc :process1)
                   )
                 x
                 )
               )
             r
             )))

(def run1 (fn [g x]
       (let [r (run g x)]
         (if (is_parsing_error? r)
           r
           (process1 r)
           )
         )
       ))

(def apply-payload-fn (fn [r]
           (clojure.walk/postwalk
             (fn [x]
               (if-let [f (-> x :payload :fn)]
                 (let [v (f x)
                       x1 (if
                     (map? v)
                     (dissoc v :fn)
                     v
                     )]
                   (if-let [the-type
                     (-> x :payload :type)]
                     {:type the-type :value x1}
                     x1
                     )
                   )
                 x
                 )
               )
             r
             )))


(def refo-concat (Y (fn [f] (fn [x]
                (cond
                  (map? x)
                  (f (if (= (:type x) :InputChar)
                    (:payload x)
                    (:value x)
                    ))

                  (sequential? x)
                  (apply str (map #(f %) x))

                  :else
                  x
                  )
                ))))

