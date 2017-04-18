(in-package :pq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-si-units)
  (define-read-macro))

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
    (q= #q(1 +- 10 % kg m ** 2 / s to the 2 / k / mol) :value 1 :error -1/10 :unit '((kilogram 1) (metre 2) (second -2) (kelvin -1) (mol -1)))))

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
    ;; Incompatible units
    (condition= #q(1 m -> s) simple-error)))

(define-test operations-test ()
  (check
    ;; Addition, exactly same units
    (q= (q+ #q(1 m) #q(2 m)) :value 3 :error 0 :unit '((m 1)))
    ;; Addition, same units, different prefixes
    (q= (q+ #q(1 m) #q(2 cm)) :value 102/100 :error 0 :unit '((m 1)))
    (q= (q+ #q(2 cm) #q(1 m)) :value 102 :error 0 :unit '((cm 1)))
    ;; Addition, different units
    (condition= (q+ #q(1 m) #q(2 s)) simple-error)

    ;; Subtraction, exactly same units
    (q= (q- #q(1 m) #q(2 m)) :value -1 :error 0 :unit '((m 1)))
    ;; Subtraction, same units, different prefixes
    (q= (q- #q(1 m) #q(2 cm)) :value 98/100 :error 0 :unit '((m 1)))
    (q= (q- #q(2 cm) #q(1 m)) :value -98 :error 0 :unit '((cm 1)))
    ;; Subtraction, different units
    (condition= (q- #q(1 m) #q(2 s)) simple-error)

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

    ;; Division with unitless number
    (q= (q/ #q(1 m) 2) :value 1/2 :error 0 :unit '((m 1)))
    (q= (q/ 2 #q(1 m)) :value 2 :error 0 :unit '((m -1)))
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
))

(define-test physical-quantities-test ()
  (check
    (definition-test)
    (conversion-test)
    (operations-test)))
