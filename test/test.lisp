(in-package :pq)

(define-test physical-quantities-test ()
  (check
    (definition-test)
    (conversion-test)
    (operations-test)
    (error-propagation-test)
    (predicate-test)
    (interface-test)
    (namespace-test)))
