(in-package :pq)

(define-condition physical-quantities-error (simple-error) () (:documentation "Generic error for the physical quantities library."))
(export '(physical-quantities-error))

(define-condition unit-definition-error (physical-quantities-error) () (:documentation "Generic error for unit/prefix definitions."))
(define-condition unit-definition-syntax-error (unit-definition-error) () (:documentation "Syntax error in the definition of a unit/prefix"))
(define-condition unit-definition-semantic-error (unit-definition-error) () (:documentation "Semantic error in the definition of a unit/prefix"))
(define-condition unit-definition-conflict-error (unit-definition-error) () (:documentation "Name conflict for the definition of a unit/prefix"))
(export '(unit-definition-error unit-definition-syntax-error unit-definition-semantic-error unit-definition-conflict-error))

(define-condition operation-undefined-error (physical-quantities-error) () (:documentation "Error for situations in which an operation is undefined"))
(export 'operation-undefined-error)

(define-condition invalid-unit-error (physical-quantities-error) () (:documentation "Generic unit error"))
(define-condition invalid-unit-operation-error (invalid-unit-error) () (:documentation "Error for situations in which the unit is invalid in a given operation"))
(define-condition invalid-unit-conversion-error (invalid-unit-error) () (:documentation "Error when converting between incompatible units"))
(define-condition invalid-unit-reference-error (invalid-unit-error) () (:documentation "Unit lookup error"))
(export '(invalid-unit-error invalid-unit-operation-error invalid-unit-conversion-error invalid-unit-reference-error))

(define-condition error-propagation-error (physical-quantities-error) () (:documentation "Error for situations in which the propagation of uncertainty is undefined"))
(export 'error-propagation-error)

(define-condition unit-syntax-error (physical-quantities-error) () (:documentation "Syntax error when referencing units"))
(export 'unit-syntax-error)

(defmacro f-error (type (&rest initargs) control &rest args)
  "Like (error ...), but allows the condition type to be specified (which is required to inherit from simple-condition)."
  `(error ',type ,@initargs :format-control ,control :format-arguments (list ,@args)))
