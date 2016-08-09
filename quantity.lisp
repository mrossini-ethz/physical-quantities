(in-package :pq)

(defclass quantity ()
  ((value :initarg :value :initform 0 :accessor value)
   (error :initarg :error :initform 0 :accessor error-direct)
   (unit :initarg :unit :initform () :accessor unit)))

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
;; FIXME: need generic functions too?
(defmethod (setf absolute-error) (error (quantity quantity))
  (setf (slot-value quantity 'error) error))
(defmethod (setf relative-error) (error (quantity quantity))
  (setf (slot-value quantity 'error) (- error)))
;; Function aliases
(setf (fdefinition 'aerr) #'absolute-error)
(setf (fdefinition '(setf aerr)) #'(setf absolute-error))
(setf (fdefinition 'rerr) #'relative-error)
(setf (fdefinition '(setf rerr)) #'(setf relative-error))

(defgeneric make-quantity (value error unit))
(defmethod make-quantity ((value number) (error number) unit)
  (make-instance 'quantity :value value :error error :unit unit))
