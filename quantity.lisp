(in-package :pq)

(defclass quantity ()
  ((value :initarg :value :initform 0 :accessor value)
   (error :initarg :error :initform 0 :accessor error-direct)
   (unit :initarg :unit :initform () :accessor unit)))
(export '(quantity value unit))

(defmethod print-object ((obj quantity) stream)
  (declare (notinline has-unit-p))
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

(declaim (inline quantityp))
(defun quantityp (object)
  (typep object 'quantity))
(export 'quantityp)

(declaim (inline has-error-p))
(defun has-error-p (quantity)
  "Checks whether a quantity has uncertainty/error."
  (not (zerop (error-direct quantity))))

(declaim (inline errorlessp))
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

(defgeneric eval-quantity (value error unit-a unit-b))
;; #q(<n> [+/- <n>] [m / s])
(defmethod eval-quantity ((value number) (error number) unit-a (unit-b (eql nil)))
  (make-quantity% :value value :error error :unit (dereference-unit unit-a)))
;; #q(<n> [+/- <n>] m / s -> km / h)
(defmethod eval-quantity ((value number) (error number) unit-a unit-b)
  (make-quantity% :value (convert-unit% value unit-a unit-b) :error (if (minusp error) error (convert-unit% error unit-a unit-b)) :unit (dereference-unit unit-b)))
;; #q(<q>)
(defmethod eval-quantity ((value quantity) (error (eql 0)) (unit-a (eql nil)) (unit-b (eql nil)))
  value)
;; #q(<q> -> km / h)
(defmethod eval-quantity ((q quantity) (error (eql 0)) unit-a (unit-b (eql nil)))
  (convert-unit% q (dereference-unit unit-a)))
;; #q(<q> km / h)
(defmethod eval-quantity ((q quantity) (error (eql 0)) (unit-a (eql nil)) unit-b)
  (convert-unit% q (dereference-unit unit-b)))

(defmacro quantity (&rest expr)
  "Function to define quantities without the reader macro."
  (let ((result (parseq 'quantity expr)))
    (unless result
      (f-error quantity-definition-syntax-error () "Syntax error in quantity definition ~{~a~^ ~}." expr))
    (destructuring-bind ((val err) unit-a unit-b) result
      `(eval-quantity ,val ,err (list ,@unit-a) (list ,@unit-b)))))
(export 'quantity)
