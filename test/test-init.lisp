(in-package :pq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-read-macros))

(define-si-units)

(defun float= (a b)
  (let ((delta (abs (- b a))))
    (typecase delta
      (double-float (< delta double-float-epsilon))
      (single-float (< delta single-float-epsilon))
      (t (= a b)))))

(defun qtest (quantity &key (value 0 value-supplied-p) (error 0 error-supplied-p) (unit () unit-supplied-p))
  (and (or (not value-supplied-p) (= (value quantity) value))
       (or (not error-supplied-p) (float= (error-direct quantity) error))
       (or (not unit-supplied-p) (units-equal (unit quantity) (if (unitp unit) unit (apply #'make-unit unit))))))
