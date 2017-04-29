(in-package :pq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-read-macro))

(define-si-units)

(defun q= (quantity &key (value 0 value-supplied-p) (error 0 error-supplied-p) (unit () unit-supplied-p))
  (and (or (not value-supplied-p) (= (value quantity) value))
       (or (not error-supplied-p) (= (error-direct quantity) error))
       (or (not unit-supplied-p) (units-equal (unit quantity) (if (unitp unit) unit (apply #'make-unit unit))))))

(define-test definition-test ()
  (check
    ;; Value only
    (q= #q(1) :value 1 :error 0 :unit ())
    ;; Value and absolute error
    (q= #q(1 +/- 0.1)  :value 1 :error 0.1 :unit ())
    ;; Value and relative error
    (q= #q(1 +/- 10 %)  :value 1 :error -1/10 :unit ())
    ;; Value and single unit factor
    (q= #q(1 m) :value 1 :error 0 :unit '((m 1)))
    (q= #q(1 / m) :value 1 :error 0 :unit '((m -1)))
    (q= #q(1 m / s) :value 1 :error 0 :unit '((m 1) (s -1)))
    (q= #q(1 m ^ 2) :value 1 :error 0 :unit '((m 2)))
    (q= #q(1 / m ^ 2) :value 1 :error 0 :unit '((m -2)))
    ;; Aliases
    (q= #q(1 metre) :value 1 :error 0 :unit '((m 1)))
    (q= #q(1 meter) :value 1 :error 0 :unit '((m 1)))
    ;; Unknown units
    (condition= #q(1 foo) simple-error)
    ;; Value and multiple unit factors
    (q= #q(1 m / s ^ 2) :value 1 :error 0 :unit '((m 1) (s -2)))
    (q= #q(1 km / h) :value 1 :error 0 :unit '((km 1) (h -1)))
    (q= #q(1 kg m ^ 2 / s ^ 2 / k / mol) :value 1 :error 0 :unit '((kilogram 1) (metre 2) (second -2) (kelvin -1) (mol -1)))
    ;; km / m should not get reduced
    (q= #q(1 km / m) :value 1 :error 0 :unit '((km 1) (m -1)))
    ;; Mixed abbreviations
    (q= #q(1 kilometre / m) :value 1 :error 0 :unit '((km 1) (m -1)))
    ;; Full syntax
    (q= #q(1 +/- 0.1 kg m ^ 2 / s ^ 2 / k / mol) :value 1 :error 0.1 :unit '((kilogram 1) (metre 2) (second -2) (kelvin -1) (mol -1)))
    (q= #q(1 +/- 10 % kg m ^ 2 / s ^ 2 / k / mol) :value 1 :error -1/10 :unit '((kilogram 1) (metre 2) (second -2) (kelvin -1) (mol -1)))
    ;; Full syntax, mixed symbol options
    (q= #q(1 +- 10 % kg m ** 2 / s to the 2 / k per mol) :value 1 :error -1/10 :unit '((kilogram 1) (metre 2) (second -2) (kelvin -1) (mol -1)))

    ;; Units
    (units-equal (mkunit m / s) (make-unit '(m 1) '(s -1)))
    (not (units-equal (mkunit m s) (make-unit '(m 1) '(s -1))))
    (not (units-equal (mkunit m / s) (make-unit '(km 1) '(s -1))))))

(define-test conversion-test ()
  (check
    ;; Different prefix
    (q= #q(1 m -> km) :value 1/1000 :error 0 :unit '((km 1)))
    ;; Different prefix, different unit
    (q= #q(1 m / s -> km / h) :value 36/10 :error 0 :unit '((km 1) (h -1)))
    ;; Different prefix, power > 1
    (q= #q(1 m ^ 3 -> cm ^ 3) :value 1000000 :error 0 :unit '((cm 3)))
    ;; Conversion factor != 1, power > 1
    (q= #q(1 hour ^ 2 -> second ^ 2) :value 12960000 :error 0 :unit '((s 2)))
    ;; Derived units
    (q= #q(1 kg m / s ^ 2 -> newton) :value 1 :error 0 :unit '((n 1)))
    (q= #q(1 newton -> kg m / s ^ 2) :value 1 :error 0 :unit '((kg 1) (m 1) (s -2)))
    ;; Special cases
    (q= #q(1 kelvin -> celsius) :value 1 :error 0 :unit '((celsius 1)))
    (q= #q(1 rad -> deg) :value (/ 180 pi) :error 0 :unit '((degree 1)))
    ;; Incompatible units
    (condition= #q(1 m -> s) simple-error)))

(define-test operations-test ()
  (check
    ;; Addition, no args
    (= (q+) 0)
    ;; Addition, unitless
    (= (q+ 2 -3) -1)
    ;; Addition, a: quantity, b: real
    (q= (q+ #q(2) -3) :value -1)
    (condition= (q+ #q(2 m) -3) simple-error)
    ;; Addition, a: real, b: quantity
    (q= (q+ 2 #q(-3)) :value -1)
    (condition= (q+ 2 #q(-3 m)) simple-error)
    ;; Addition, exactly same units
    (q= (q+ #q(1 m) #q(2 m)) :value 3 :error 0 :unit '((m 1)))
    ;; Addition, same units, different prefixes
    (q= (q+ #q(1 m) #q(2 cm)) :value 102/100 :error 0 :unit '((m 1)))
    (q= (q+ #q(2 cm) #q(1 m)) :value 102 :error 0 :unit '((cm 1)))
    ;; Addition, different units
    (condition= (q+ #q(1 m) #q(2 s)) simple-error)

    ;; Subtraction, one arg
    (= (q- 3) -3)
    (q= (q- #q(3 +/- 0.2 m)) :value -3 :error 0.2 :unit '((m 1)))
    ;; Subtraction, real numbers
    (= (q- 2 -3) 5)
    ;; Subtraction, exactly same units
    (q= (q- #q(1 m) #q(2 m)) :value -1 :error 0 :unit '((m 1)))
    ;; Subtraction, same units, different prefixes
    (q= (q- #q(1 m) #q(2 cm)) :value 98/100 :error 0 :unit '((m 1)))
    (q= (q- #q(2 cm) #q(1 m)) :value -98 :error 0 :unit '((cm 1)))
    ;; Subtraction, different units
    (condition= (q- #q(1 m) #q(2 s)) simple-error)

    ;; Multiplication, no args
    (= (q*) 1)
    ;; Multiplication, a: real, b: real
    (= (q* 2 -3) -6)
    ;; Multiplication, a: real, b: quantity
    (q= (q* 2 #q(-3 +/- 1/10 m)) :value -6 :error 1/5 :unit '((m 1)))
    ;; Multiplication, a: quantity, b: real
    (q= (q* #q(2 +/- 1/10 m) -3) :value -6 :error 3/10 :unit '((m 1)))
    ;; Multiplication with unitless number
    (q= (q* #q(1 m) 2) :value 2 :error 0 :unit '((m 1)))
    (q= (q* 2 #q(1 m)) :value 2 :error 0 :unit '((m 1)))
    ;; Multiplication, exactly same units
    (q= (q* #q(1 m) #q(1 m)) :value 1 :error 0 :unit '((m 2)))
    ;; Multiplication, same units, different prefixes
    (q= (q* #q(1 m) #q(2 cm)) :value 2 :error 0 :unit '((m 1) (cm 1)))
    (q= (q* #q(1 cm) #q(2 m)) :value 2 :error 0 :unit '((m 1) (cm 1)))
    ;; Multiplication, same units, different notation
    (q= (q* #q(1 kg m / s ^ 2) #q(2 n)) :value 2 :error 0 :unit '((kg 1) (m 1) (s -2) (n 1)))
    ;; Multiplication, different units
    (q= (q* #q(1 kg m / s ^ 2) #q(2 m / s)) :value 2 :error 0 :unit '((kg 1) (m 2) (s -3)))

    ;; Division, one arg
    (= (q/ -2) -1/2)
    (q= (q/ #q(-2 m / s ^ 2)) :value -1/2 :error 0 :unit '((s 2) (m -1)))
    ;; Division, a: real, b: real
    (= (q/ 2 -3) -2/3)
    ;; Division, a: quantity, b: real
    (q= (q/ #q(1 +/- 2/10 m) 2) :value 1/2 :error 1/10 :unit '((m 1)))
    (q= (q/ #q(1 +/- 10 % m) 2) :value 1/2 :error -1/10 :unit '((m 1)))
    ;; Division, a: real, b: quantity
    (q= (q/ 2 #q(1 +/- 1/10 m)) :value 2 :unit '((m -1)))
    (q= (q/ 2 #q(1 +/- 10 % m)) :value 2 :error -1/10 :unit '((m -1)))
    ;; Division, exactly same units
    (q= (q/ #q(1 m) #q(1 m)) :value 1 :error 0 :unit '())
    ;; Division, same units, different prefixes
    (q= (q/ #q(1 m) #q(2 cm)) :value 1/2 :error 0 :unit '((m 1) (cm -1)))
    (q= (q/ #q(1 cm) #q(2 m)) :value 1/2 :error 0 :unit '((m -1) (cm 1)))
    ;; Division, same units, different notation
    (q= (q/ #q(1 kg m / s ^ 2) #q(2 n)) :value 1/2 :error 0 :unit '((kg 1) (m 1) (s -2) (n -1)))
    ;; Division, different units
    (q= (q/ #q(1 kg m / s ^ 2) #q(2 m / s)) :value 1/2 :error 0 :unit '((kg 1) (s -1)))

    ;; Power, unitless
    (= (qpow 2 3) 8)
    (= (qpow 2 0) 1)
    (= (qpow 0 0) 1)
    (condition= (qpow 0 -1) division-by-zero)
    ;; Power, Base: quantity, Exponent: integer
    (q= (qpow #q(2 +/- 0.1 m) 0) :value 1 :unit '())
    (q= (qpow #q(2 +/- 0.1 m) 3) :value 8 :unit '((m 3)))
    (q= (qpow #q(2 +/- 0.1 m) -3) :value 1/8 :unit '((m -3)))
    (q= (qpow #q(0 +/- 0.1 m) 3) :value 0 :error 0 :unit '((m 3)))
    (condition= (qpow #q(0 +/- 0.1 m) -3) division-by-zero)
    (q= (qpow #q(-2 +/- 0.1 m) 3) :value -8 :unit '((m 3)))
    (q= (qpow #q(-2 +/- 0.1 m) -3) :value -1/8 :unit '((m -3)))
    ;; Power, Base: real, Exponent: quantity
    (condition= (qpow 2 #q(3 m)) simple-error)
    (condition= (qpow 2 #q(3 +/- 0.1)) simple-error)
    (condition= (qpow 2 #q(1/3)) simple-error)
    (q= (qpow 2 #q(3)) :value 8 :error 0 :unit ())
    (q= (qpow 2 #q(-3)) :value 1/8 :unit ())
    (q= (qpow 0 #q(3)) :value 0 :unit ())
    (condition= (qpow 0 #q(-3)) division-by-zero)
    (q= (qpow -2 #q(3)) :value -8 :unit ())
    (q= (qpow -2 #q(-3)) :value -1/8 :unit ())
    ;; Power, Base: quantity, Exponent: quantity
    ;; unitless
    (q= (qpow #q(2) #q(3)) :value 8 :unit '())
    (q= (qpow #q(2 +/- 0.1) #q(3)) :value 8 :unit '())
    (condition= (qpow #q(2) #q(1/3)) simple-error)
    ;; with units
    (q= (qpow #q(2 m) #q(3)) :value 8 :unit '((m 3)))
    (q= (qpow #q(2 +/- 0.1 m) #q(3)) :value 8 :unit '((m 3)))
    (q= (qpow #q(2 +/- 0.1 m) #q(-3)) :value 1/8 :unit '((m -3)))
    (q= (qpow #q(0 +/- 0.1 m) #q(3)) :value 0 :error 0 :unit '((m 3)))
    (condition= (qpow #q(0 +/- 0.1 m) #q(-3)) division-by-zero)
    (q= (qpow #q(-2 +/- 0.1 m) #q(3)) :value -8 :unit '((m 3)))
    (q= (qpow #q(-2 +/- 0.1 m) #q(-3)) :value -1/8 :unit '((m -3)))
    (condition= (qpow #q(2 m) #q(3 +/- 0.1)) simple-error)

    ;; Root, no units
    (= (qroot 27 3) 3)
    (= (qroot -27 3) -3)
    (= (qroot 16 4) 2)
    (condition= (qroot -16 4) simple-error)
    (condition= (qroot 2 0) simple-error)
    (condition= (qroot 2 -1) simple-error)
    ;; Root, radicand: quantity, degree: integer
    ;; unitess
    (q= (qroot #q(27) 3) :value 3 :error 0 :unit ())
    (q= (qroot #q(27 +/- 0.2) 3) :value 3 :unit ())
    (q= (qroot #q(-27) 3) :value -3 :error 0 :unit ())
    (q= (qroot #q(16) 4) :value 2 :error 0 :unit ())
    (q= (qroot #q(0 +/- 0.1) 2) :value 0 :error 0 :unit ())
    (condition= (qroot #q(-16) 4) simple-error)
    (condition= (qroot #q(2) 0) simple-error)
    (condition= (qroot #q(2) -1) simple-error)
    ;; with units
    (q= (qroot #q(27 m ^ 3) 3) :value 3 :error 0 :unit '((m 1)))
    (q= (qroot #q(27 +/- 0.1 m ^ 3) 3) :value 3 :unit '((m 1)))
    (condition= (qroot #q(27 m ^ 4) 3) simple-error)
    ;; Root, radicand: real, degree: quantity
    ;; unitless, errorless
    (q= (qroot 27 #q(3)) :value 3 :error 0 :unit ())
    (q= (qroot -27 #q(3)) :value -3 :error 0 :unit ())
    (q= (qroot 16 #q(4)) :value 2 :error 0 :unit ())
    (condition= (qroot -16 #q(4)) simple-error)
    ;; not unitless, not errorless
    (condition= (qroot 27 #q(3 +/- 0.1)) simple-error)
    (condition= (qroot 27 #q(3 m)) simple-error)
    ;; Root, radicand: quantity, degree: quantity
    ;; degree unitless, errorless
    (q= (qroot #q(27) #q(3)) :value 3 :error 0 :unit ())
    (q= (qroot #q(27 +/- 0.2) #q(3)) :value 3 :unit ())
    (q= (qroot #q(-27) #q(3)) :value -3 :error 0 :unit ())
    (q= (qroot #q(16) #q(4)) :value 2 :error 0 :unit ())
    (condition= (qroot #q(-16) #q(4)) simple-error)
    (condition= (qroot #q(2) #q(0)) simple-error)
    (condition= (qroot #q(2) #q(-1)) simple-error)
    ;; degree not unitless, errorless
    (condition= (qroot #q(27) #q(3 +/- 0.1)) simple-error)
    (condition= (qroot #q(27) #q(3 m)) simple-error)
    ;; radicand with units
    (q= (qroot #q(27 m ^ 3 / s ^ 3) #q(3)) :value 3 :error 0 :unit '((m 1) (s -1)))
    (q= (qroot #q(27 +/- 0.1 m ^ 3 / s ^ 3) #q(3)) :value 3 :unit '((m 1) (s -1)))
    (condition= (qroot #q(27 m ^ 3 / s ^ 4) #q(3)) simple-error)

    ;; Square root
    (= (qsqrt 9) 3)
    (q= (qsqrt #q(9 m ^ 2)) :value 3 :unit '((m 1)))

    ;; Exponentiation qexp, exponent: real
    (= (qexp -3) (exp -3))
    ;; Exponentiation qexp, exponent: quantity
    (q= (qexp #q(-3)) :value (exp -3) :error 0 :unit ())
    (q= (qexp #q(-3 +/- 0.1)) :value (exp -3) :unit ())
    (condition= (qexp #q(1 m)) simple-error)

    ;; Exponentiation qexpt, base: real, exponent: real
    (= (qexpt -2 -3) -1/8)
    (= (qexpt 0 0) 1)
    ;; Exponentiation qexpt, base: real, exponent: quantity
    (q= (qexpt -2 #q(-3)) :value -1/8 :error 0 :unit ())
    (q= (qexpt 2 #q(-3 +/- 0.1)) :value 1/8 :unit ())
    (condition= (qexpt -2 #q(-3 +/- 0.1)) simple-error)
    (condition= (qexpt 2 #q(3 m)) simple-error)
    ;; Exponentiation qexpt, base: quantity, exponent: integer
    (q= (qexpt #q(-2 m) -3) :value -1/8 :error 0 :unit '((m -3)))
    (q= (qexpt #q(-2 +/- 0.1 m) -3) :value -1/8 :unit '((m -3)))
    ;; Exponentiation qexpt, base: quantity, exponent: ratio
    (q= (qexpt #q(27 m ^ 3) 1/3) :value 3 :error 0 :unit '((m 1)))
    (q= (qexpt #q(27 m ^ 3) 2/3) :value 9 :error 0 :unit '((m 2)))
    (condition= (qexpt #q(27 m ^ 3) 2/5) simple-error)
    ;; Exponentiation qexpt, base: quantity, exponent: float
    (q= (qexpt #q(0) 0.33) :value 0 :error 0)
    (q= (qexpt #q(27) 0.33) :value (expt 27 0.33))
    (condition= (qexpt #q(27 m) 0.33) simple-error)
    ;; Exponentiation qexpt, base: quantity, exponent: quantity
    (condition= (qexpt #q(27) #q(3 m)) simple-error)
    (q= (qexpt #q(0) #q(1/3)) :value 0 :error 0 :unit ())
    (q= (qexpt #q(27) #q(1/3)) :value 3 :error 0 :unit ())
    (q= (qexpt #q(27) #q(1/3 +/- 0.01)) :value 3 :unit ())
    (condition= (qexpt #q(27 m) #q(1/3 +/- 0.01)) simple-error)
    (condition= (qexpt #q(-27) #q(1/3 +/- 0.01)) simple-error)
    (q= (qexpt #q(27 +/- 0.2) #q(1/3 +/- 0.01)) :value 3 :unit ())

    ;; Logarithm qln
    (= (qln 2) (log 2))
    (q= (qln #q(2 +/- 0.1)) :value (log 2))

    ;; Logarithm qlog, reals
    (= (qlog 100 10) 2)
    ;; Logarithm qlog, number: quantity, base: real
    (q= (qlog #q(100 +/- 0.1) 10) :value 2 :unit ())
    (condition= (qlog #q(100 +/- 0.1 m) 10) simple-error)
    ;; Logarithm qlog, number: real, base: quantity
    (q= (qlog 100 #q(10 +/- 0.1)) :value 2 :unit ())
    (condition= (qlog 100 #q(10 +/- 0.1 m)) simple-error)
    ;; Logarithm qlog, number: quantity, base: quantity
    (q= (qlog #q(100 +/- 1) #q(10 +/- 0.1)) :value 2 :unit ())
    (condition= (qlog #q(100 +/- 1 m)  #q(10 +/- 0.1 m)) simple-error)

    ;; Trigonometric functions
    ;; qsin
    (= (qsin 2) (sin 2))
    (q= (qsin #q(2 +/- 0.1)) :value (sin 2) :unit ())
    (condition= (qsin #q(2 +/- 0.1 m)) simple-error)
    ;; qcos
    (= (qcos 2) (cos 2))
    (q= (qcos #q(2 +/- 0.1)) :value (cos 2) :unit ())
    (condition= (qcos #q(2 +/- 0.1 m)) simple-error)
    ;; qtan
    (= (qtan 2) (tan 2))
    (q= (qtan #q(2 +/- 0.1)) :value (tan 2) :unit ())
    (condition= (qtan #q(2 +/- 0.1 m)) simple-error)

    ;; Inverse trigonometric functions
    ;; qasin
    (= (qasin 1/2) (asin 1/2))
    (q= (qasin #q(1/2 +/- 0.1)) :value (asin 1/2) :unit ())
    (condition= (qasin #q(1/2 +/- 0.1 m)) simple-error)
    ;; qacos
    (= (qacos 1/2) (acos 1/2))
    (q= (qacos #q(1/2 +/- 0.1)) :value (acos 1/2) :unit ())
    (condition= (qacos #q(1/2 +/- 0.1 m)) simple-error)
    ;; qatan
    (= (qatan 1/2) (atan 1/2))
    (q= (qatan #q(1/2 +/- 0.1)) :value (atan 1/2) :unit ())
    (condition= (qatan #q(1/2 +/- 0.1 m)) simple-error)

    ;; Hyperbolic functions
    ;; qsinh
    (= (qsinh 2) (sinh 2))
    (q= (qsinh #q(2 +/- 0.1)) :value (sinh 2) :unit ())
    (condition= (qsinh #q(2 +/- 0.1 m)) simple-error)
    ;; qcosh
    (= (qcosh 2) (cosh 2))
    (q= (qcosh #q(2 +/- 0.1)) :value (cosh 2) :unit ())
    (condition= (qcosh #q(2 +/- 0.1 m)) simple-error)
    ;; qtanh
    (= (qtanh 2) (tanh 2))
    (q= (qtanh #q(2 +/- 0.1)) :value (tanh 2) :unit ())
    (condition= (qtanh #q(2 +/- 0.1 m)) simple-error)

    ;; Inverse hyperbolic functions
    ;; qasinh
    (= (qasinh 1/2) (asinh 1/2))
    (q= (qasinh #q(1/2 +/- 0.1)) :value (asinh 1/2) :unit ())
    (condition= (qasinh #q(1/2 +/- 0.1 m)) simple-error)
    ;; qacosh
    (= (qacosh 1/2) (acosh 1/2))
    (q= (qacosh #q(1/2 +/- 0.1)) :value (acosh 1/2) :unit ())
    (condition= (qacosh #q(1/2 +/- 0.1 m)) simple-error)
    ;; qatanh
    (= (qatanh 1/2) (atanh 1/2))
    (q= (qatanh #q(1/2 +/- 0.1)) :value (atanh 1/2) :unit ())
    (condition= (qatanh #q(1/2 +/- 0.1 m)) simple-error)

    ;; Absolute value
    (= (qabs -3) 3)
    (q= (qabs #q(-3 +/- 5 m)) :value 3 :error 5 :unit '((m 1)))))

(define-test interface-test ()
  (check
    ;; make-quantity
    (q= (make-quantity) :value 0 :error 0 :unit ())
    (q= (make-quantity :value 1) :value 1 :error 0 :unit ())
    (q= (make-quantity :error 1) :value 0 :error 1 :unit ())
    (q= (make-quantity :error 1 :error-type :relative) :value 0 :error -1 :unit ())
    (q= (make-quantity :value 2 :error 0.2 :unit '((m 1) (s -2))) :value 2 :error 0.2 :unit '((m 1) (s -2)))
    (condition= (make-quantity :value #C(1 1)) simple-error)
    (condition= (make-quantity :error -1) simple-error)
    (condition= (make-quantity :error-type :pq) simple-error)

    ;; make-unit
    (q= (make-quantity :value 2 :error 0.2 :unit (make-unit '(m 1) '(s -1))) :value 2 :error 0.2 :unit '((m 1) (s -1)))
    (q= (make-quantity :value 2 :error 0.2 :unit (make-unit '("M" 1) '("S" -1))) :value 2 :error 0.2 :unit '((m 1) (s -1)))
    (condition= (make-unit '(m 1 2)) simple-error)

    ;; convert-unit
    (q= (convert-unit #q(10 +/- 1/10 m / s) '((km 1) (h -1))) :value 36 :error 36/100 :unit '((km 1) (h -1)))
    (q= (convert-unit #q(10 +/- 1/10 m / s) (make-unit '(km 1) '(h -1))) :value 36 :error 36/100 :unit '((km 1) (h -1)))
    (condition= (convert-unit 10  '((km 1) (h -1))) simple-error)))

(define-test namespace-test ()
  (check
    (with-local-units
      (condition= #q(1 km) simple-error))
    (with-saved-units
      (q= #q(1 km) :value 1 :error 0 :unit '((kilometre 1))))
    (with-saved-units
      (define-unit foo :abbrev fo)
      (q/ #q(1 foo) #q(1 second)))
    (condition= #q((with-saved-units
                     (define-unit foo :abbrev fo)
                     (q/ #q(1 foo) #q(1 second)))
                   -> metre)
                simple-error)))

(define-test predicate-test ()
  (check
    ;; units-equal
    (units-equal (mkunit m / s) (mkunit / s m))
    (not (units-equal (mkunit km / h) (mkunit m / s)))
    (units-equal (mkunit m ^ 2 / s / m) (mkunit m / s))
    (not (units-equal (mkunit newton) (mkunit kg m / s ^ 2)))
    ;; units-equalp
    (units-equalp (mkunit newton) (mkunit m kg / s ^ 2))
    (not (units-equalp (mkunit km / h) (mkunit m / s)))
    ;; units-convertible
    (units-convertible (mkunit km / h) (mkunit m / s))
    (units-convertible (mkunit kw h) (mkunit joule))
    (not (units-convertible (mkunit km / h) (mkunit m / s ^ 2)))))

(define-test physical-quantities-test ()
  (check
    (definition-test)
    (conversion-test)
    (operations-test)
    (predicate-test)
    (interface-test)
    (namespace-test)))
