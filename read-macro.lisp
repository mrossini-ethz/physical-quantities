(in-package :pq)

(defgeneric eval-quantity (value error unit-a unit-b))
;; #q(<n> [+/- <n>] [m / s])
(defmethod eval-quantity ((value number) (error number) unit-a (unit-b (eql nil)))
  (make-quantity% :value value :error error :unit (dereference-unit unit-a)))
;; #q(<n> [+/- <n>] m / s -> km / h)
(defmethod eval-quantity ((value number) (error number) unit-a unit-b)
  (make-quantity% :value (convert-unit% value unit-a unit-b) :error (if (minusp error) error (convert-unit% error unit-a unit-b)) :unit (dereference-unit unit-b)))
;; #q(<q>)
(defmethod eval-quantity ((value quantity) (error (eql 0)) (unit-a (eql nil)) (unit-b (eql nil)))
  (value value))
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

(defun read-quantity (stream char1 char2)
  "The read macro #q(...) is an abbreviation for (quantity ...)."
  (declare (ignore char1 char2))
  `(quantity ,@(read stream t nil t)))

(defun define-read-macro ()
  "Lets the user define the #q(...) read macro."
  (set-dispatch-macro-character #\# #\q #'read-quantity))
(export 'define-read-macro)
