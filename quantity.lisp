(in-package :pq)

(defclass quantity ()
  ((value :initarg :value :initform 0 :accessor value)
   (error :initarg :error :initform 0 :accessor error-direct)
   (unit :initarg :unit :initform () :accessor unit)))
(export '(quantity value unit))

(defmethod print-object ((obj quantity) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a~:[ +/- ~a~;~*~]~:[~*~; ~a~]"
            (value obj)
            (zerop (error-direct obj))
            (if (minusp (error-direct obj)) (format nil "~a %" (* 100 (relative-error obj))) (absolute-error obj))
            (has-unit-p obj)
            (str-unit (unit obj)))))

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

(defun quantityp (object)
  (eql (type-of object) 'quantity))
(export 'quantityp)

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
    (f-error quantity-definition-semantic-error () "VALUE in (MAKE-QUANTITY :VALUE VALUE) must be a real number."))
  (unless (and (realp error) (>= error 0))
    (f-error quantity-definition-semantic-error () "ERROR in (MAKE-QUANTITY :ERROR ERROR) must be a positive real number."))
  (unless (have error-type '(:absolute :relative))
    (f-error quantity-definition-syntax-error () "ERROR-TYPE in (MAKE-QUANTITY :ERROR-TYPE ERROR-TYPE) must be either :ABSOLUTE or :RELATIVE."))
  (make-instance 'quantity :value value :error (if (eql error-type :absolute) error (- error)) :unit (if (unitp unit) unit (apply #'make-unit unit))))
(export 'make-quantity)
