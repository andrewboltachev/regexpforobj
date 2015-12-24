(ns regexpforobj.core_test)
#?(:clj
  (defmacro test_run_func1 [n g x y]
    (deftest n
      (is (= 
            (run g x)
            y
            ))))

  ; (defmacro mytest [name] `(deftest ~name true))

  (defmacro test_run_func [n g x y]
    `(deftest ~n
      (is (= 
          ~y
          (run ~g ~x)
          ))))

  (test_run_func
    Char_basic
    (Char "a")
    [(InputChar "a")]
    (InputChar "a"))


  (test_run_func
    Char_basic_tr
    (Char "b")
    [(InputChar "b")]
    (InputChar "b"))


  (test_run_func
    Char_new
    (Char "a")
    [(InputChar "b")]
    (ParsingError :char {:expected "a" :found "b"}))


  (test_run_func
    Char_tail
    (Char "a")
    [(InputChar "a") (InputChar "b")]
    (ParsingError :tail {:tail [(InputChar "b")]}))


  (test_run_func
    Char_short
    (Char "a") 
    []
    (ParsingError :too-short-char {:rest (Char "a")})
    )


  (test_run_func
    SeqSingle1
    (Seq [(Char "a")])
    [(InputChar "a")]
    (SeqNode [(InputChar "a")])
    )



  (test_run_func
    SeqTwo
    (Seq [(Char "a") (Char "b")])
    [(InputChar "a") (InputChar "b")]
    (SeqNode [(InputChar "a") (InputChar "b")])
    )


  (test_run_func
    SeqLong
    (Seq [(Char "a") (Char "b") (Char "c")])
    [(InputChar "a") (InputChar "b")]
    (ParsingError :too-short-char {:rest (Char "c")})
    )


  (test_run_func
    SeqNeg
    (Seq [(Char "a") (Char "c")])
    [(InputChar "a") (InputChar "b")]
    (ParsingError :char {:expected "c" :found "b"})
    )


  (test_run_func
    Or_basic
    (Or [(Char "a") (Char "b") (Char "c")])
    [(InputChar "a")]
    (SeqNode (InputChar "a"))
    )


  (test_run_func
    Or_neg
    (Or [(Char "a") (Char "b") (Char "c")])
    [(InputChar "d")]
    (ParsingError :or-fail)
    )


  (test_run_func
    Star_basic
    (Star (Char "a"))
    [(InputChar "a") (InputChar "a") (InputChar "a") (InputChar "a")]
    (SeqNode [(InputChar "a") (InputChar "a") (InputChar "a") (InputChar "a")])
    )


  (test_run_func
    Star_basic1
    (Star (Char "a"))
    []
    (SeqNode [])
    )


  (test_run_func
    MayBe_empty
    (MayBe (Char "a"))
    []
    (SeqNode [])
    )


  (test_run_func
    MayBe_full
    (MayBe (Char "a"))
    [(InputChar "a")]
    (SeqNode (InputChar "a"))
    )


  (test_run_func
    example00001
    (Or [
        (Seq [
              (Char "aa1")
              ])
        (Star 
              (Char "bb1")
              )
        ])
    [(InputChar "aa1")]
    (SeqNode (SeqNode [(InputChar "aa1")]))
    )

  ; payloads
  ; Char

  (test_run_func
    payload_Char
    (Char "a")
    [(InputChar "a" {:a 1 :b 2})]
    (InputChar "a" {:a 1 :b 2})
    )


  (test_run_func
    payload_Seq
    (Seq [(Char "a") (Char "b") (Char "c")] {:a 1 :b 2})
    [(InputChar "a" {:a 1 :b 2}) (InputChar "b" {:a 3 :b 4}) (InputChar "c" {:a 5 :b 6})]
    (SeqNode [(InputChar "a" {:a 1 :b 2}) (InputChar "b" {:a 3 :b 4}) (InputChar "c" {:a 5 :b 6})] {:a 1 :b 2})
    )


  (test_run_func
    payload_Or
    (Or [(Char "a") (Char "b") (Char "c")] {:a 1 :b 2})
    [(InputChar "a" {:a 1 :b 2})]
    (SeqNode (InputChar "a" {:a 1 :b 2}) {:a 1 :b 2})
    )


  (test_run_func
    payload_Or2
    (Star (Char "a") {:a 1 :b 2})
    []
    (SeqNode [] {:a 1 :b 2})
    )
  )



;(time (run-tests 'zarnidict0005.grammar-checker))








