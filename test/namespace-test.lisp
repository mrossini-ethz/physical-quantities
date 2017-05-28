(in-package :pq)

(define-test namespace-test ()
  (check
    (with-local-units
      (condition= #q(1 km) invalid-unit-reference-error))
    (with-saved-units
      (qtest #q(1 km) :value 1 :error 0 :unit '((|kilometre| 1))))))
