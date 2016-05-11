(ns regexpforobj.core
  #?(
     :cljs
     (:require-macros [regexpforobj.core :refer [grammar-symbol]])

     :clj
      (:use clojure.test)
      )
  )


#?(:clj
     (require
      '[io.aviso.ansi :as font]
       )
      (use 'aprint.core)
    )

#?(:cljs
    (enable-console-print!))

; grammar
(defmacro grammar-symbol [name_]
  `(defn ~name_
     ([~'value & [~'payload]] {:type ~(keyword name_) :value ~'value :payload ~'payload})
     )
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
               (let [r (list (symbol (name (:type m))) (grammar_pretty (:value m)))
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

(defn process [g x & [level]]
  (let [level (or level 0)
        process (fn [g x] (process g x (inc level)))
        process (fn [& args]
                  (let [result (apply process args)]
                    (println font/blue-font
                             (with-out-str
                               (aprint (map grammar_pretty args))
                               )
                             font/reset-font)
                    (println font/red-font
                             (with-out-str
                               (aprint (grammar_pretty result))
                               )
                             font/reset-font)
                    )
                  )
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

(defn run [g x]
  (let [returned (process g x)]
    (if (is_parsing_error? returned)
      returned
      (let [[result tail] returned]
        (if (= (count tail) 0)
          result
          (ParsingError :tail {:tail tail})
          )))))

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

