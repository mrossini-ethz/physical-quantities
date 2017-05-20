(in-package :pq)

(defmacro quantity (&rest expr)
  "Function to define quantities without the reader macro."
  (let ((result (parseq 'quantity expr)))
    (unless result
      (f-error quantity-definition-syntax-error () "Syntax error in quantity definition ~{~a~^ ~}." expr))
    (destructuring-bind ((val err) unit-a unit-b) result
      `(eval-quantity ,val ,err (list ,@unit-a) (list ,@unit-b)))))
(export 'quantity)

(defun read-quantity (stream char1 char2)
  "The read macro #q(...) is an abbreviation for (quantity ...) which does not transform lowercase symbols to uppercase."
  (declare (ignore char1 char2))
  `(quantity ,@(let ((*readtable* (copy-readtable *readtable*)))
                 ;; Change the readtable such that case is preserved
                 (setf (readtable-case *readtable*) :preserve)
                 (read stream t nil t))))

(defun define-read-macro ()
  "Lets the user define the #q(...) read macro."
  (set-dispatch-macro-character #\# #\q #'read-quantity))
(export 'define-read-macro)
