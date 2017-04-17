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
    (q= (q/ #q(1 kg m / s ^ 2) #q(2 m / s)) :value 1/2 :error 0 :unit '((kg 1) (s -1)))))

(define-test physical-quantities-test ()
  (check
    (definition-test)
    (conversion-test)
    (operations-test)))
