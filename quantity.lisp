(in-package :pq)

(defclass quantity ()
  ((value :initarg :value :initform 0 :accessor value)
   (error :initarg :error :initform 0 :accessor error-direct)
   (unit :initarg :unit :initform () :accessor unit)))
(export '(quantity value unit))

(defmethod print-object ((obj quantity) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "VALUE: ~a, ERROR: ~a, UNIT: ~a" (value obj) (if (minusp (error-direct obj)) (format nil "~a %" (* 100 (relative-error obj))) (absolute-error obj)) (print-unit (unit obj)))))

;; Error functions
(defun ae (val err)
  (if (minusp err) (* val (- err)) err))
(defun re (val err)
  (if (plusp err) (/ err val) (- err)))

;; Accessor functions for error
(defgeneric absolute-error (quantity))
(defgeneric relative-error (quantity))
(defmethod absolute-error ((quantity quantity))
  (with-slots (value error) quantity
    (ae value error)))
(defmethod relative-error ((quantity quantity))
  (with-slots (value error) quantity
    (re value error)))
;; Setf functions for error
(defgeneric (setf absolute-error) (error quantity))
(defgeneric (setf relative-error) (error quantity))
(defmethod (setf absolute-error) (error (quantity quantity))
  (setf (slot-value quantity 'error) error))
(defmethod (setf relative-error) (error (quantity quantity))
  (setf (slot-value quantity 'error) (- error)))
;; Function aliases
(setf (fdefinition 'aerr) #'absolute-error)
(setf (fdefinition '(setf aerr)) #'(setf absolute-error))
(setf (fdefinition 'rerr) #'relative-error)
(setf (fdefinition '(setf rerr)) #'(setf relative-error))
(export '(absolute-error relative-error aerr rerr))

(defun has-error-p (quantity)
  "Checks whether a quantity has uncertainty/error."
  (not (zerop (error-direct quantity))))

(defun errorlessp (quantity)
  "Checks whether a quantity is without uncertainty/error."
  (not (has-error-p quantity)))

;; Internal function to make quantities
(defun make-quantity% (&key (value 0) (error 0) (unit nil))
  (make-instance 'quantity :value value :error error :unit unit))

(defun make-quantity (&key (value 0) (error 0) (error-type :absolute) (unit ()))
  (unless (realp value)
    (error "The value of a quantity must be a real number."))
  (unless (and (realp error) (>= error 0))
    (error "The error of a quantity must be a positive real number."))
  (unless (have error-type '(:absolute :relative))
    (error "The error type for a quantity must be either :absolute or :relative."))
  (make-instance 'quantity :value value :error (if (eql error-type :absolute) error (- error)) :unit (if (unitp unit) unit (apply #'make-unit unit))))
(export 'make-quantity)
