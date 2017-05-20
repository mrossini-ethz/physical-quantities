(in-package :pq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-read-macros))

(define-si-units)

(defun qtest (quantity &key (value 0 value-supplied-p) (error 0 error-supplied-p) (unit () unit-supplied-p))
  (and (or (not value-supplied-p) (= (value quantity) value))
       (or (not error-supplied-p) (= (error-direct quantity) error))
       (or (not unit-supplied-p) (units-equal (unit quantity) (if (unitp unit) unit (apply #'make-unit unit))))))

(define-test definition-test ()
  (check
    ;; Value only
    (qtest #q(1) :value 1 :error 0 :unit ())
    (qtest #q(#q(2 +/- 0.1 metre)) :value 2 :error 0.1 :unit '((|m| 1)))
    ;; Value and absolute error
    (qtest #q(1 +/- 0.1)  :value 1 :error 0.1 :unit ())
    ;; Value and relative error
    (qtest #q(1 +/- 10 %)  :value 1 :error -1/10 :unit ())
    ;; Value and single unit factor
    (qtest #q(1 m) :value 1 :error 0 :unit '((|m| 1)))
    (qtest #q(1 "m") :value 1 :error 0 :unit '((|m| 1)))
    (qtest #q(1 / m) :value 1 :error 0 :unit '((|m| -1)))
    (qtest #q(1 m / s) :value 1 :error 0 :unit '((|m| 1) (|s| -1)))
    (qtest #q(1 m ^ 2) :value 1 :error 0 :unit '((|m| 2)))
    (qtest #q(1 metre squared) :value 1 :error 0 :unit '((|m| 2)))
    (qtest #q(1 square metre) :value 1 :error 0 :unit '((|m| 2)))
    (qtest #q(1 metre cubed) :value 1 :error 0 :unit '((|m| 3)))
    (qtest #q(1 cubic metre) :value 1 :error 0 :unit '((|m| 3)))
    (qtest #q(1 / m ^ 2) :value 1 :error 0 :unit '((|m| -2)))
    (qtest #q(1 / metre squared) :value 1 :error 0 :unit '((|m| -2)))
    (qtest #q(1 / square metre) :value 1 :error 0 :unit '((|m| -2)))
    (qtest #q(1 / metre cubed) :value 1 :error 0 :unit '((|m| -3)))
    (qtest #q(1 / cubic metre) :value 1 :error 0 :unit '((|m| -3)))
    ;; Aliases
    (qtest #q(1 metre) :value 1 :error 0 :unit '((|m| 1)))
    (qtest #q(1 meter) :value 1 :error 0 :unit '((|m| 1)))
    ;; Unknown units
    (condition= #q(1 foo) invalid-unit-reference-error)
    ;; Value and multiple unit factors
    (qtest #q(1 m / s ^ 2) :value 1 :error 0 :unit '((|m| 1) (|s| -2)))
    (qtest #q(1 km / h) :value 1 :error 0 :unit '((|km| 1) (|h| -1)))
    (qtest #q(1 kg m ^ 2 / s ^ 2 / K / mol) :value 1 :error 0 :unit '((|kilogram| 1) (|metre| 2) (|second| -2) (|kelvin| -1) (|mol| -1)))
    ;; km / m should not get reduced
    (qtest #q(1 km / m) :value 1 :error 0 :unit '((|km| 1) (|m| -1)))
    ;; Mixed abbreviations
    (qtest #q(1 kilometre / m) :value 1 :error 0 :unit '((|km| 1) (|m| -1)))
    ;; Full syntax
    (qtest #q(1 +/- 0.1 kg m ^ 2 / s ^ 2 / K / mol) :value 1 :error 0.1 :unit '((|kilogram| 1) (|metre| 2) (|second| -2) (|kelvin| -1) (|mol| -1)))
    (qtest #q(1 +/- 10 % kg m ^ 2 / s ^ 2 / K / mol) :value 1 :error -1/10 :unit '((|kilogram| 1) (|metre| 2) (|second| -2) (|kelvin| -1) (|mol| -1)))
    ;; Full syntax, mixed symbol options
    (qtest #q(1 +- 10 % kg m ** 2 / s to the 2 / K per mol) :value 1 :error -1/10 :unit '((|kilogram| 1) (|metre| 2) (|second| -2) (|kelvin| -1) (|mol| -1)))

    ;; Units
    (units-equal (mkunit |m| / |s|) (make-unit '(|m| 1) '(|s| -1)))
    (units-equal (mkunit "m" / "s") (make-unit '("m" 1) '("s" -1)))
    (not (units-equal (mkunit |m| |s|) (make-unit '(|m| 1) '(|s| -1))))
    (not (units-equal (mkunit |m| / |s|) (make-unit '(|km| 1) '(|s| -1))))))

(define-test conversion-test ()
  (check
    ;; Different prefix
    (qtest #q(1 m -> km) :value 1/1000 :error 0 :unit '((|km| 1)))
    ;; Different prefix, different unit
    (qtest #q(1 m / s -> km / h) :value 36/10 :error 0 :unit '((|km| 1) (|h| -1)))
    ;; Different prefix, power > 1
    (qtest #q(1 m ^ 3 -> cm ^ 3) :value 1000000 :error 0 :unit '((|cm| 3)))
    ;; Conversion factor != 1, power > 1
    (qtest #q(1 hour ^ 2 -> second ^ 2) :value 12960000 :error 0 :unit '((|s| 2)))
    ;; Derived units
    (qtest #q(1 kg m / s ^ 2 -> newton) :value 1 :error 0 :unit '((|N| 1)))
    (qtest #q(1 newton -> kg m / s ^ 2) :value 1 :error 0 :unit '((|kg| 1) (|m| 1) (|s| -2)))
    ;; Special cases
    (qtest #q(1 kelvin -> celsius) :value 1 :error 0 :unit '((|celsius| 1)))
    (qtest #q(1 rad -> deg) :value (/ 180 pi) :error 0 :unit '((|degree| 1)))
    ;; Incompatible units
    (condition= #q(1 m -> s) invalid-unit-conversion-error)))

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
    (qtest (q- #q(3 +/- 0.2 m)) :value -3 :error 0.2 :unit '((|m| 1)))
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
    (qtest (q* 2 #q(-3 +/- 1/10 m)) :value -6 :error 0.2 :unit '((|m| 1)))
    ;; Multiplication, a: quantity, b: real
    (qtest (q* #q(2 +/- 1/10 m) -3) :value -6 :error 0.3 :unit '((|m| 1)))
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
    (qtest (q/ #q(1 +/- 2/10 m) 2) :value 1/2 :error 0.1 :unit '((|m| 1)))
    (qtest (q/ #q(1 +/- 10 % m) 2) :value 1/2 :error 0.05 :unit '((|m| 1)))
    ;; Division, a: real, b: quantity
    (qtest (q/ 2 #q(1 +/- 1/10 m)) :value 2 :unit '((|m| -1)))
    (qtest (q/ 2 #q(1 +/- 10 % m)) :value 2 :error 0.2 :unit '((|m| -1)))
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
    (qtest (qabs #q(-3 +/- 5 m)) :value 3 :error 5 :unit '((|m| 1)))))

(define-test interface-test ()
  (check
    ;; make-quantity
    (qtest (make-quantity) :value 0 :error 0 :unit ())
    (qtest (make-quantity :value 1) :value 1 :error 0 :unit ())
    (qtest (make-quantity :error 1) :value 0 :error 1 :unit ())
    (qtest (make-quantity :error 1 :error-type :relative) :value 0 :error -1 :unit ())
    (qtest (make-quantity :value 2 :error 0.2 :unit '((|m| 1) (|s| -2))) :value 2 :error 0.2 :unit '((|m| 1) (|s| -2)))
    (qtest (make-quantity :value 2 :error 0.2 :unit '(("m" 1) ("s" -2))) :value 2 :error 0.2 :unit '((|m| 1) (|s| -2)))
    (condition= (make-quantity :value #C(1 1)) quantity-definition-semantic-error)
    (condition= (make-quantity :error -1) quantity-definition-semantic-error)
    (condition= (make-quantity :error-type :pq) quantity-definition-syntax-error)

    ;; make-unit
    (qtest (make-quantity :value 2 :error 0.2 :unit (make-unit '(|m| 1) '(|s| -1))) :value 2 :error 0.2 :unit '((|m| 1) (|s| -1)))
    (qtest (make-quantity :value 2 :error 0.2 :unit (make-unit '("m" 1) '("s" -1))) :value 2 :error 0.2 :unit '((|m| 1) (|s| -1)))
    (condition= (make-unit '(|m| 1 2)) quantity-definition-syntax-error)

    ;; convert-unit
    (qtest (convert-unit #q(10 +/- 1/10 m / s) '((|km| 1) (|h| -1))) :value 36 :error 36/100 :unit '((|km| 1) (|h| -1)))
    (qtest (convert-unit #q(10 +/- 1/10 m / s) (make-unit '(|km| 1) '(|h| -1))) :value 36 :error 36/100 :unit '((|km| 1) (|h| -1)))
    (condition= (convert-unit 10 '((|km| 1) (|h| -1))) invalid-unit-conversion-error)))

(define-test namespace-test ()
  (check
    (with-local-units
      (condition= #q(1 km) invalid-unit-reference-error))
    (with-saved-units
      (qtest #q(1 km) :value 1 :error 0 :unit '((|kilometre| 1))))))

(define-test predicate-test ()
  (check
    ;; qequal real, real
    (qequal 3 3)
    (not (qequal 3 2))
    ;; qequal real, quantity
    (qequal 3 #q(3))
    (qequal #q(3) 3)
    (not (qequal #q(3 +/- 0 m) 3))
    (not (qequal 3 #q(3 +/- 0 m)))
    (not (qequal #q(3 +/- 1) 3))
    (not (qequal 3 #q(3 +/- 1)))
    ;; qequal quantity, quantity
    (qequal #q(3 +/- 2 m) #q(3 +/- 2 m))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 2.0 m)))
    (not (qequal #q(3 +/- 2 m) #q(300 +/- 200 cm)))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 3 m)))
    (not (qequal #q(3 +/- 2 m) #q(2 +/- 2 m)))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 2 s)))
    (not (qequal #q(1 newton) #q(1 kg m / s ^ 2)))
    (not (qequal #q(1 kelvin) #q(1 celsius)))

    ;; qequalp real, real
    (qequalp 3 3)
    (not (qequalp 3 2))
    ;; qequalp real, quantity
    (qequalp 3 #q(3))
    (qequalp #q(3) 3)
    (not (qequalp #q(3 +/- 0 m) 3))
    (not (qequalp 3 #q(3 +/- 0 m)))
    (not (qequalp #q(3 +/- 1) 3))
    (not (qequalp 3 #q(3 +/- 1)))
    ;; qequalp quantity, quantity
    (qequalp #q(3 +/- 2 m) #q(3 +/- 2 m))
    (qequalp #q(3 +/- 2 m) #q(3 +/- 2.0 m))
    (not (qequalp #q(3 +/- 2 m) #q(300 +/- 200 cm)))
    (not (qequalp #q(3 +/- 2 m) #q(3 +/- 3 m)))
    (not (qequalp #q(3 +/- 2 m) #q(2 +/- 2 m)))
    (not (qequalp #q(3 +/- 2 m) #q(3 +/- 2 s)))
    (qequalp #q(1 newton) #q(1 kg m / s ^ 2))
    (qequalp #q(1 kelvin) #q(1 celsius))

    ;; q= real, real
    (q= 3 3)
    (not (q= 3 2))
    ;; q= real, quantity
    (q= #q(3) 3)
    (q= 3 #q(3))
    (not (q= #q(3) 2))
    (not (q= 2 #q(3)))
    (condition= (q= #q(3 m) 3) invalid-unit-conversion-error)
    (condition= (q= 3 #q(3 m)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 25 to 28 never (q= #q(30 +/- 1) (+ v 0.035))))
    (identity (loop for v upfrom 28 to 30 always (q= #q(30 +/- 1) (+ v 0.045))))
    (identity (loop for v upfrom 30 to 32 always (q= #q(30 +/- 1) (- v 0.045))))
    (identity (loop for v upfrom 32 to 35 never (q= #q(30 +/- 1) (- v 0.035))))
    ;; q= quantity, quantity
    (q= #q(3) #q(3))
    (not (q= #q(3) #q(4)))
    (condition= (q= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (not (q= #q(30 +/- 2 m) #q(21 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(22 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(23 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(24 +/- 2 m)))
    (q= #q(30 +/- 2 m) #q(25 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(26 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(27 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(28 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(29 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(30 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(31 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(32 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(33 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(34 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(35 +/- 2 m))
    (not (q= #q(30 +/- 2 m) #q(36 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(37 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(38 +/- 2 m)))
    (not (q= #q(30 +/- 3 m) #q(39 +/- 3 m)))

    ;; q/=
    (q/= #q(30 +/- 1 m) #q(25.61 +/- 2 m))
    (not (q/= #q(30 +/- 1 m) #q(25.62 +/- 2 m)))
    (not (q/= #q(30 +/- 1 m) #q(34.38 +/- 2 m)))
    (q/= #q(30 +/- 1 m) #q(34.39 +/- 2 m))

    ;; q< real, real
    (q< 2 3)
    (not (q< 2 2))
    (not (q< 3 2))
    ;; q< quantity, real
    (condition= (q< #q(3 m) 3) invalid-unit-conversion-error)
    (q< #q(3) 4)
    (not (q< #q(3) 3))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q< q (+ v 0.64))))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q< q (+ v 0.65))))
    ;; q< real, quantity
    (condition= (q< 3 #q(3 m)) invalid-unit-conversion-error)
    (q< 3 #q(4))
    (not (q< 3 #q(3)))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q< (- v 0.65) q)))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q< (- v 0.64) q)))
    ;; q< quantity, quantity
    (condition= (q< #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 203 with q = #q(200 +/- 2) never (q< q #q((+ V 0.667) +/- 1))))
    (identity (loop for v upfrom 203 to 210 with q = #q(200 +/- 2) always (q< q #q((+ V 0.689) +/- 1))))

    ;; q<= real, real
    (q<= 2 3)
    (q<= 2 2)
    (not (q<= 3 2))
    ;; q<= quantity, real
    (condition= (q<= #q(3 m) 3) invalid-unit-conversion-error)
    (q<= #q(3) 4)
    (q<= #q(3) 3)
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q<= q (+ v 0.64))))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q<= q (+ v 0.65))))
    ;; q<= real, quantity
    (condition= (q<= 3 #q(3 m)) invalid-unit-conversion-error)
    (q<= 3 #q(4))
    (q<= 3 #q(3))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q<= (- v 0.65) q)))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q<= (- v 0.64) q)))
    ;; q<= quantity, quantity
    (condition= (q<= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 203 with q = #q(200 +/- 2) never (q<= q #q((+ V 0.667) +/- 1))))
    (identity (loop for v upfrom 203 to 210 with q = #q(200 +/- 2) always (q<= q #q((+ V 0.689) +/- 1))))

    ;; q> real, real
    (q> 3 2)
    (not (q> 2 2))
    (not (q> 2 3))
    ;; q> quantity, real
    (condition= (q> #q(3 m) 3) invalid-unit-conversion-error)
    (q> #q(4) 3)
    (not (q> #q(3) 3))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q> q (- v 0.65))))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q> q (- v 0.64))))
    ;; q> real, quantity
    (condition= (q> 3 #q(3 m)) invalid-unit-conversion-error)
    (q> 4 #q(3))
    (not (q> 3 #q(3)))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q> (+ v 0.64) q)))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q> (+ v 0.65) q)))
    ;; q> quantity, quantity
    (condition= (q> #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 197 with q = #q(200 +/- 2) always (q> q #q((- V 0.689) +/- 1))))
    (identity (loop for v upfrom 197 to 210 with q = #q(200 +/- 2) never (q> q #q((- V 0.667) +/- 1))))

    ;; q>= real, real
    (q>= 3 2)
    (q>= 2 2)
    (not (q>= 2 3))
    ;; q>= quantity, real
    (condition= (q>= #q(3 m) 3) invalid-unit-conversion-error)
    (q>= #q(4) 3)
    (q>= #q(3) 3)
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q>= q (- v 0.65))))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q>= q (- v 0.64))))
    ;; q>= real, quantity
    (condition= (q>= 3 #q(3 m)) invalid-unit-conversion-error)
    (q>= 4 #q(3))
    (q>= 3 #q(3))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q>= (+ v 0.64) q)))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q>= (+ v 0.65) q)))
    ;; q>= quantity, quantity
    (condition= (q>= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 197 with q = #q(200 +/- 2) always (q>= q #q((- V 0.689) +/- 1))))
    (identity (loop for v upfrom 197 to 210 with q = #q(200 +/- 2) never (q>= q #q((- V 0.667) +/- 1))))

    ;; units-equal
    (units-equal (mkunit |m| / |s|) (mkunit / |s| |m|))
    (not (units-equal (mkunit |km| / |h|) (mkunit |m| / |s|)))
    (units-equal (mkunit |m| ^ 2 / |s| / |m|) (mkunit |m| / |s|))
    (not (units-equal (mkunit |newton|) (mkunit |kg| |m| / |s| ^ 2)))
    ;; units-equalp
    (units-equalp (mkunit |newton|) (mkunit |m| |kg| / |s| ^ 2))
    (not (units-equalp (mkunit |km| / |h|) (mkunit |m| / |s|)))
    ;; units-convertible
    (units-convertible (mkunit |km| / |h|) (mkunit |m| / |s|))
    (units-convertible (mkunit |kW| |h|) (mkunit |joule|))
    (not (units-convertible (mkunit |km| / |h|) (mkunit |m| / |s| ^ 2)))))

(define-test physical-quantities-test ()
  (check
    (definition-test)
    (conversion-test)
    (operations-test)
    (predicate-test)
    (interface-test)
    (namespace-test)))
