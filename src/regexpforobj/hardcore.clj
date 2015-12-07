(ns regexpforobj.hardcore)

(require '[clojure.core.typed :as t])


(defmacro grammar-symbol [name]
  `(defn ~name    
     ([~'value] (~name ~'value nil))
     ([~'value ~'payload] {:type ~(keyword name) :value ~'value :payload ~'payload})
     )            
  ) 
  

(t/defalias InputChar-type
  (t/HMap
    :mandatory {:value t/Any
     :payload t/Any
     :type (t/Val :InputChar)
     }
    :complete? true
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
                 [Input-type -> Input-type]
                 [Input-type t/Any -> Input-type]
                 ))

(grammar-symbol SeqNode)

