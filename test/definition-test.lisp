(in-package :pq)

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
