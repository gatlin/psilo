; a little arithmetic language

(::= Arithmetic (a)
  (: Value a)
  (: Add (Arithmetic a) (Arithmetic a))
  (: Mul (Arithmetic a) (Arithmetic a)))

(: arith (-> (Arithmetic Integer) Integer))
(= arith ('(Value v)) v)
(= arith ('(Add x y))
  (+ (arith x)
     (arith y)))
(= arith ('(Mul x y))
  (* (arith x)
     (arith y)))

(let
  ((a '(Add
         (Mul
           (Value 5)
           (Value 6))
         (Mul
           (Value 2)
           (Value 3)))))
  (arith a))

; => 36
