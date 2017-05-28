(in-package :pq)

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
