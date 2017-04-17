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

(define-test physical-quantities-test ()
  (check
    (definition-test)))
