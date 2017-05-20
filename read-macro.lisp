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
  "The read macro #q(...) is an abbreviation for (quantity ...) which does not transform lowercase symbols to uppercase for unit factors."
  (declare (ignore char2))
  (let (items)
    ;; Read the opening bracket
    (when (char/= (read-char stream) #\()
      (error "Error reading quantity. #~c must be followed by a list." char1))
    ;; Perform minimal parsing of the stream to decide whether or not to perserve the letter case
    (when (char= (peek-char t stream t) #\))
      (error "Error reading quantity: #~c()" char1))
    ;; Read the value of the quantity (do not preserve case)
    (push (read stream t nil t) items)
    ;; Test for the closing bracket
    (unless (char= (peek-char t stream t) #\))
      ;; Read the next symbol (preserve case because it's either the symbol +/- or +- or a unit)
      (let ((*readtable* (copy-readtable *readtable*)))
        (setf (readtable-case *readtable*) :preserve)
        (push (read stream t nil t) items))
      ;; Check whether an uncertainty is specified
      (when (or (eql (first items) '+/-) (eql (first items) '+-))
        ;; Read the uncertainty (do not preserve case)
        (when (char= (peek-char t stream t) #\))
          (error "Error reading quantity #~c(~a ~a): ~a must be followed by an uncertainty/error." char1 (second items) (first items) (first items)))
        (push (read stream t nil t) items))
      ;; Read the rest of the list (preserving case)
      (let ((*readtable* (copy-readtable *readtable*)))
        (setf (readtable-case *readtable*) :preserve)
        (loop until (char= (peek-char t stream t) #\)) do
             (push (read stream t nil t) items))))
    ;; Read the closing bracket
    (read-char stream)
    ;; Expand the list into a macro call
    `(quantity ,@(nreverse items))))

(defun define-read-macro ()
  "Lets the user define the #q(...) read macro."
  (set-dispatch-macro-character #\# #\q #'read-quantity))
(export 'define-read-macro)
