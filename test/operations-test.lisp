(in-package :pq)

(define-test operations-test ()
  (check
    ;; Addition, no args
    (qtest (q+) :value 0 :error 0 :unit nil)
    ;; Addition, unitless
    (qtest (q+ 2 -3) :value -1 :error 0 :unit nil)
    ;; Addition, a: quantity, b: real
    (qtest (q+ #q(2) -3) :value -1)
    (condition= (q+ #q(2 m) -3) invalid-unit-conversion-error)
    ;; Addition, a: real, b: quantity
    (qtest (q+ 2 #q(-3)) :value -1)
    (condition= (q+ 2 #q(-3 m)) invalid-unit-conversion-error)
    ;; Addition, exactly same units
    (qtest (q+ #q(1 m) #q(2 m)) :value 3 :error 0 :unit '((|m| 1)))
    ;; Addition, same units, different prefixes
    (qtest (q+ #q(1 m) #q(2 cm)) :value 102/100 :error 0 :unit '((|m| 1)))
    (qtest (q+ #q(2 cm) #q(1 m)) :value 102 :error 0 :unit '((|cm| 1)))
    ;; Addition, different units
    (condition= (q+ #q(1 m) #q(2 s)) invalid-unit-conversion-error)

    ;; Subtraction, one arg
    (qtest (q- 3) :value -3 :error 0 :unit nil)
    (qtest (q- #q(3 +/- 0.2d0 m)) :value -3 :error 0.2d0 :unit '((|m| 1)))
    ;; Subtraction, real numbers
    (qtest (q- 2 -3) :value 5 :error 0 :unit nil)
    ;; Subtraction, a: quantity, b: real
    (qtest (q- 2 #q(-3)) :value 5)
    (condition= (q- 2 #q(-3 m)) invalid-unit-conversion-error)
    ;; Subtraction, a: quantity, b: real
    (qtest (q- #q(2) -3) :value 5)
    (condition= (q- #q(2 m) -3) invalid-unit-conversion-error)
    ;; Subtraction, exactly same units
    (qtest (q- #q(1 m) #q(2 m)) :value -1 :error 0 :unit '((|m| 1)))
    ;; Subtraction, same units, different prefixes
    (qtest (q- #q(1 m) #q(2 cm)) :value 98/100 :error 0 :unit '((|m| 1)))
    (qtest (q- #q(2 cm) #q(1 m)) :value -98 :error 0 :unit '((|cm| 1)))
    ;; Subtraction, different units
    (condition= (q- #q(1 m) #q(2 s)) invalid-unit-conversion-error)

    ;; Multiplication, no args
    (qtest (q*) :value 1 :error 0 :unit nil)
    ;; Multiplication, a: real, b: real
    (qtest (q* 2 -3) :value -6 :error 0 :unit nil)
    ;; Multiplication, a: real, b: quantity
    (qtest (q* 2d0 #q(-3d0 +/- 1/10 m)) :value -6d0 :error 0.2d0 :unit '((|m| 1)))
    ;; Multiplication, a: quantity, b: real
    (qtest (q* #q(2d0 +/- 1/10 m) -3d0) :value -6d0 :error 0.3d0 :unit '((|m| 1)))
    ;; Multiplication with unitless number
    (qtest (q* #q(1 m) 2) :value 2 :error 0 :unit '((|m| 1)))
    (qtest (q* 2 #q(1 m)) :value 2 :error 0 :unit '((|m| 1)))
    ;; Multiplication, exactly same units
    (qtest (q* #q(1 m) #q(1 m)) :value 1 :error 0 :unit '((|m| 2)))
    ;; Multiplication, same units, different prefixes
    (qtest (q* #q(1 m) #q(2 cm)) :value 2 :error 0 :unit '((|m| 1) (|cm| 1)))
    (qtest (q* #q(1 cm) #q(2 m)) :value 2 :error 0 :unit '((|m| 1) (|cm| 1)))
    ;; Multiplication, same units, different notation
    (qtest (q* #q(1 kg m / s ^ 2) #q(2 N)) :value 2 :error 0 :unit '((|kg| 1) (|m| 1) (|s| -2) (|N| 1)))
    ;; Multiplication, different units
    (qtest (q* #q(1 kg m / s ^ 2) #q(2 m / s)) :value 2 :error 0 :unit '((|kg| 1) (|m| 2) (|s| -3)))

    ;; Division, one arg
    (qtest (q/ -2) :value -1/2 :error 0 :unit nil)
    (qtest (q/ #q(-2 m / s ^ 2)) :value -1/2 :error 0 :unit '((|s| 2) (|m| -1)))
    ;; Division, a: real, b: real
    (qtest (q/ 2 -3) :value -2/3 :error 0 :unit nil)
    ;; Division, a: quantity, b: real
    (qtest (q/ #q(1d0 +/- 2/10 m) 2) :value 1/2 :error 0.1d0 :unit '((|m| 1)))
    (qtest (q/ #q(1d0 +/- 10d0 % m) 2) :value 1/2 :error 0.05d0 :unit '((|m| 1)))
    ;; Division, a: real, b: quantity
    (qtest (q/ 2 #q(1 +/- 1/10 m)) :value 2 :unit '((|m| -1)))
    (qtest (q/ 2 #q(1 +/- 10d0 % m)) :value 2 :error 0.2d0 :unit '((|m| -1)))
    ;; Division, exactly same units
    (qtest (q/ #q(1 m) #q(1 m)) :value 1 :error 0 :unit '())
    ;; Division, same units, different prefixes
    (qtest (q/ #q(1 m) #q(2 cm)) :value 1/2 :error 0 :unit '((|m| 1) (|cm| -1)))
    (qtest (q/ #q(1 cm) #q(2 m)) :value 1/2 :error 0 :unit '((|m| -1) (|cm| 1)))
    ;; Division, same units, different notation
    (qtest (q/ #q(1 kg m / s ^ 2) #q(2 N)) :value 1/2 :error 0 :unit '((|kg| 1) (|m| 1) (|s| -2) (|N| -1)))
    ;; Division, different units
    (qtest (q/ #q(1 kg m / s ^ 2) #q(2 m / s)) :value 1/2 :error 0 :unit '((|kg| 1) (|s| -1)))

    ;; Power, unitless
    (qtest (qpow 2 3) :value 8 :error 0 :unit nil)
    (qtest (qpow 2 0) :value 1 :error 0 :unit nil)
    (qtest (qpow 0 0) :value 1 :error 0 :unit nil)
    (condition= (qpow 0 -1) division-by-zero)
    ;; Power, Base: quantity, Exponent: integer
    (qtest (qpow #q(2 +/- 0.1 m) 0) :value 1 :error 0 :unit '())
    (qtest (qpow #q(2 +/- 0.1 m) 3) :value 8 :unit '((|m| 3)))
    (qtest (qpow #q(2 +/- 0.1 m) -3) :value 1/8 :unit '((|m| -3)))
    (qtest (qpow #q(0 +/- 0.1 m) 3) :value 0 :error 0 :unit '((|m| 3)))
    (condition= (qpow #q(0 +/- 0.1 m) -3) division-by-zero)
    (qtest (qpow #q(-2 +/- 0.1 m) 3) :value -8 :unit '((|m| 3)))
    (qtest (qpow #q(-2 +/- 0.1 m) -3) :value -1/8 :unit '((|m| -3)))
    ;; Power, Base: real, Exponent: quantity
    (condition= (qpow 2 #q(3 m)) invalid-unit-operation-error)
    (condition= (qpow 2 #q(3 +/- 0.1)) operation-undefined-error)
    (condition= (qpow 2 #q(1/3)) operation-undefined-error)
    (qtest (qpow 2 #q(3)) :value 8 :error 0 :unit ())
    (qtest (qpow 2 #q(-3)) :value 1/8 :unit ())
    (qtest (qpow 0 #q(3)) :value 0 :unit ())
    (condition= (qpow 0 #q(-3)) division-by-zero)
    (qtest (qpow -2 #q(3)) :value -8 :unit ())
    (qtest (qpow -2 #q(-3)) :value -1/8 :unit ())
    ;; Power, Base: quantity, Exponent: quantity
    ;; unitless
    (qtest (qpow #q(2) #q(3)) :value 8 :unit '())
    (qtest (qpow #q(2 +/- 0.1) #q(3)) :value 8 :unit '())
    (condition= (qpow #q(2) #q(1/3)) operation-undefined-error)
    ;; with units
    (qtest (qpow #q(2 m) #q(3)) :value 8 :unit '((|m| 3)))
    (condition= (qpow #q(2 m) #q(3 m)) invalid-unit-operation-error)
    (qtest (qpow #q(2 +/- 0.1 m) #q(3)) :value 8 :unit '((|m| 3)))
    (qtest (qpow #q(2 +/- 0.1 m) #q(-3)) :value 1/8 :unit '((|m| -3)))
    (qtest (qpow #q(0 +/- 0.1 m) #q(3)) :value 0 :error 0 :unit '((|m| 3)))
    (condition= (qpow #q(0 +/- 0.1 m) #q(-3)) division-by-zero)
    (qtest (qpow #q(-2 +/- 0.1 m) #q(3)) :value -8 :unit '((|m| 3)))
    (qtest (qpow #q(-2 +/- 0.1 m) #q(-3)) :value -1/8 :unit '((|m| -3)))
    (condition= (qpow #q(2 m) #q(3 +/- 0.1)) operation-undefined-error)

    ;; Root, no units
    (qtest (qroot 27 3) :value 3 :error 0 :unit nil)
    (qtest (qroot -27 3) :value -3 :error 0 :unit nil)
    (qtest (qroot 16 4) :value 2 :error 0 :unit nil)
    (condition= (qroot -16 4) operation-undefined-error)
    (condition= (qroot 2 0) operation-undefined-error)
    (condition= (qroot 2 -1) operation-undefined-error)
    ;; Root, radicand: quantity, degree: integer
    ;; unitess
    (qtest (qroot #q(27) 3) :value 3 :error 0 :unit ())
    (qtest (qroot #q(27 +/- 0.2) 3) :value 3 :unit ())
    (qtest (qroot #q(-27) 3) :value -3 :error 0 :unit ())
    (qtest (qroot #q(16) 4) :value 2 :error 0 :unit ())
    (qtest (qroot #q(0 +/- 0.1) 2) :value 0 :error 0 :unit ())
    (condition= (qroot #q(-16) 4) operation-undefined-error)
    (condition= (qroot #q(2) 0) operation-undefined-error)
    (condition= (qroot #q(2) -1) operation-undefined-error)
    ;; with units
    (qtest (qroot #q(27 m ^ 3) 3) :value 3 :error 0 :unit '((|m| 1)))
    (qtest (qroot #q(27 +/- 0.1 m ^ 3) 3) :value 3 :unit '((|m| 1)))
    (condition= (qroot #q(27 m ^ 4) 3) invalid-unit-operation-error)
    ;; Root, radicand: real, degree: quantity
    ;; unitless, errorless
    (qtest (qroot 27 #q(3)) :value 3 :error 0 :unit ())
    (qtest (qroot -27 #q(3)) :value -3 :error 0 :unit ())
    (qtest (qroot 16 #q(4)) :value 2 :error 0 :unit ())
    (condition= (qroot -16 #q(4)) operation-undefined-error)
    ;; not unitless, not errorless
    (condition= (qroot 27 #q(3 +/- 0.1)) operation-undefined-error)
    (condition= (qroot 27 #q(3 m)) invalid-unit-operation-error)
    ;; Root, radicand: quantity, degree: quantity
    ;; degree unitless, errorless
    (qtest (qroot #q(27) #q(3)) :value 3 :error 0 :unit ())
    (qtest (qroot #q(27 +/- 0.2) #q(3)) :value 3 :unit ())
    (qtest (qroot #q(-27) #q(3)) :value -3 :error 0 :unit ())
    (qtest (qroot #q(16) #q(4)) :value 2 :error 0 :unit ())
    (condition= (qroot #q(-16) #q(4)) operation-undefined-error)
    (condition= (qroot #q(2) #q(0)) operation-undefined-error)
    (condition= (qroot #q(2) #q(-1)) operation-undefined-error)
    ;; degree not unitless, errorless
    (condition= (qroot #q(27) #q(3 +/- 0.1)) operation-undefined-error)
    (condition= (qroot #q(27) #q(3 m)) invalid-unit-operation-error)
    ;; radicand with units
    (qtest (qroot #q(27 m ^ 3 / s ^ 3) #q(3)) :value 3 :error 0 :unit '((|m| 1) (|s| -1)))
    (qtest (qroot #q(27 +/- 0.1 m ^ 3 / s ^ 3) #q(3)) :value 3 :unit '((|m| 1) (|s| -1)))
    (condition= (qroot #q(27 m ^ 3 / s ^ 4) #q(3)) invalid-unit-operation-error)

    ;; Square root
    (qtest (qsqrt 9) :value 3 :error 0 :unit nil)
    (qtest (qsqrt #q(9 m ^ 2)) :value 3 :unit '((|m| 1)))
    ;; Special cases
    (qtest (qsqrt #q(9 joule / kilogram)) :value 3 :unit '((|m| 1) (|s| -1)))
    (qtest (qsqrt #q(1 au metre / second ^ 2)) :value (sqrt 149597870700) :unit '((|m| 1) (|s| -1)))

    ;; Exponentiation qexp, exponent: real
    (qtest (qexp -3) :value (exp -3) :error 0 :unit nil)
    ;; Exponentiation qexp, exponent: quantity
    (qtest (qexp #q(-3)) :value (exp -3) :error 0 :unit ())
    (qtest (qexp #q(-3 +/- 0.1)) :value (exp -3) :unit ())
    (condition= (qexp #q(1 m)) invalid-unit-operation-error)

    ;; Exponentiation qexpt, base: real, exponent: real
    (qtest (qexpt -2 -3) :value -1/8 :error 0 :unit ())
    (qtest (qexpt 0 0) :value 1 :error 0 :unit ())
    ;; Exponentiation qexpt, base: real, exponent: quantity
    (qtest (qexpt -2 #q(-3)) :value -1/8 :error 0 :unit ())
    (qtest (qexpt 2 #q(-3 +/- 0.1)) :value 1/8 :unit ())
    (condition= (qexpt -2 #q(-3 +/- 0.1)) error-propagation-error)
    (condition= (qexpt 2 #q(3 m)) invalid-unit-operation-error)
    ;; Exponentiation qexpt, base: quantity, exponent: integer
    (qtest (qexpt #q(-2 m) -3) :value -1/8 :error 0 :unit '((|m| -3)))
    (qtest (qexpt #q(-2 +/- 0.1 m) -3) :value -1/8 :unit '((|m| -3)))
    ;; Exponentiation qexpt, base: quantity, exponent: ratio
    (qtest (qexpt #q(27 m ^ 3) 1/3) :value 3 :error 0 :unit '((|m| 1)))
    (qtest (qexpt #q(27 m ^ 3) 2/3) :value 9 :error 0 :unit '((|m| 2)))
    (qtest (qexpt #q(4 joule / kilogram) 3/2) :value 8 :error 0 :unit '((|m| 3) (|s| -3)))
    (condition= (qexpt #q(27 m ^ 3) 2/5) invalid-unit-operation-error)
    ;; Exponentiation qexpt, base: quantity, exponent: float
    (qtest (qexpt #q(0) 0.33) :value 0 :error 0)
    (qtest (qexpt #q(27) 0.33) :value (expt 27 0.33))
    (condition= (qexpt #q(27 m) 0.33) invalid-unit-operation-error)
    ;; Exponentiation qexpt, base: quantity, exponent: quantity
    (condition= (qexpt #q(27) #q(3 m)) invalid-unit-operation-error)
    (qtest (qexpt #q(0) #q(1/3)) :value 0 :error 0 :unit ())
    (qtest (qexpt #q(27) #q(1/3)) :value 3 :error 0 :unit ())
    (qtest (qexpt #q(27) #q(1/3 +/- 0.01)) :value 3 :unit ())
    (condition= (qexpt #q(27 m) #q(1/3 +/- 0.01)) invalid-unit-operation-error)
    (condition= (qexpt #q(-27) #q(1/3 +/- 0.01)) error-propagation-error)
    (qtest (qexpt #q(27 +/- 0.2) #q(1/3 +/- 0.01)) :value 3 :unit ())

    ;; Logarithm qln
    (qtest (qln 2) :value (log 2) :error 0 :unit ())
    (qtest (qln #q(2 +/- 0.1)) :value (log 2))
    (condition= (qln #q(2 m)) invalid-unit-operation-error)

    ;; Logarithm qlog, reals
    (qtest (qlog 100 10) :value 2 :error 0 :unit ())
    ;; Logarithm qlog, number: quantity, base: real
    (qtest (qlog #q(100 +/- 0.1) 10) :value 2 :unit ())
    (condition= (qlog #q(100 +/- 0.1 m) 10) invalid-unit-operation-error)
    ;; Logarithm qlog, number: real, base: quantity
    (qtest (qlog 100 #q(10 +/- 0.1)) :value 2 :unit ())
    (condition= (qlog 100 #q(10 +/- 0.1 m)) invalid-unit-operation-error)
    ;; Logarithm qlog, number: quantity, base: quantity
    (qtest (qlog #q(100 +/- 1) #q(10 +/- 0.1)) :value 2 :unit ())
    (condition= (qlog #q(100 +/- 1) #q(10 +/- 0.1 m)) invalid-unit-operation-error)
    (condition= (qlog #q(100 +/- 1 m)  #q(10 +/- 0.1 m)) invalid-unit-operation-error)

    ;; Trigonometric functions
    ;; qsin
    (qtest (qsin 2) :value (sin 2) :error 0 :unit ())
    (qtest (qsin #q(2 +/- 0.1)) :value (sin 2) :unit ())
    (condition= (qsin #q(2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qcos
    (qtest (qcos 2) :value (cos 2) :error 0 :unit ())
    (qtest (qcos #q(2 +/- 0.1)) :value (cos 2) :unit ())
    (condition= (qcos #q(2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qtan
    (qtest (qtan 2) :value (tan 2) :error 0 :unit ())
    (qtest (qtan #q(2 +/- 0.1)) :value (tan 2) :unit ())
    (condition= (qtan #q(2 +/- 0.1 m)) invalid-unit-operation-error)

    ;; Inverse trigonometric functions
    ;; qasin
    (qtest (qasin 1/2) :value (asin 1/2) :error 0 :unit ())
    (qtest (qasin #q(1/2 +/- 0.1)) :value (asin 1/2) :unit ())
    (condition= (qasin #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qacos
    (qtest (qacos 1/2) :value (acos 1/2) :error 0 :unit ())
    (qtest (qacos #q(1/2 +/- 0.1)) :value (acos 1/2) :unit ())
    (condition= (qacos #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qatan
    (qtest (qatan 1/2) :value (atan 1/2) :error 0 :unit ())
    (qtest (qatan #q(1/2 +/- 0.1)) :value (atan 1/2) :unit ())
    (condition= (qatan #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)

    ;; Hyperbolic functions
    ;; qsinh
    (qtest (qsinh 2) :value (sinh 2) :error 0 :unit ())
    (qtest (qsinh #q(2 +/- 0.1)) :value (sinh 2) :unit ())
    (condition= (qsinh #q(2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qcosh
    (qtest (qcosh 2) :value (cosh 2) :error 0 :unit ())
    (qtest (qcosh #q(2 +/- 0.1)) :value (cosh 2) :unit ())
    (condition= (qcosh #q(2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qtanh
    (qtest (qtanh 2) :value (tanh 2) :error 0 :unit ())
    (qtest (qtanh #q(2 +/- 0.1)) :value (tanh 2) :unit ())
    (condition= (qtanh #q(2 +/- 0.1 m)) invalid-unit-operation-error)

    ;; Inverse hyperbolic functions
    ;; qasinh
    (qtest (qasinh 1/2) :value (asinh 1/2) :error 0 :unit ())
    (qtest (qasinh #q(1/2 +/- 0.1)) :value (asinh 1/2) :unit ())
    (condition= (qasinh #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qacosh
    (qtest (qacosh 1/2) :value (acosh 1/2) :error 0 :unit ())
    (qtest (qacosh #q(1/2 +/- 0.1)) :value (acosh 1/2) :unit ())
    (condition= (qacosh #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)
    ;; qatanh
    (qtest (qatanh 1/2) :value (atanh 1/2) :error 0 :unit ())
    (qtest (qatanh #q(1/2 +/- 0.1)) :value (atanh 1/2) :unit ())
    (condition= (qatanh #q(1/2 +/- 0.1 m)) invalid-unit-operation-error)

    ;; Absolute value
    (qtest (qabs -3) :value 3 :error 0 :unit ())
    (qtest (qabs #q(-3 +/- 5 m)) :value 3 :error 5 :unit '((|m| 1)))
    (condition= (qabs #q(0 +/- 0.1)) error-propagation-error)))
