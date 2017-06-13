(in-package :pq)

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
    (condition= (convert-unit #q(10) '((|km| 1) (|h| -1))) invalid-unit-conversion-error)
    (condition= (convert-unit 10 '((|km| 1) (|h| -1))) error)))
