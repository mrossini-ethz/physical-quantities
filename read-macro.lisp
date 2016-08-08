(in-package :pq)

(defmacro cond-table ((&rest keyforms) &body cases)
  ;; FIXME: Keyforms should be evaluated only once and in order!
  (let ((num (list-length keyforms)))
    `(cond
       ,@(loop for cas in cases collect
              `((and ,@(loop for n below num collect
                            (if (eql (nth n (first cas)) t)
                                t
                                `(funcall #',(nth n (first cas)) ,(nth n keyforms)))))
                (progn ,@(rest cas)))))))

(defrule value () form)
(defrule unit-factor () (and (? (or '/ 'per)) (not '->) (? (and (or '^ '** (and 'to 'the)) form)))
  (:destructure (per symb exponent) `(list ',symb ,(if exponent (if per `(- ,(second exponent)) (second exponent)) (if per -1 1)))))
(defrule error () (and (or '+/- '+-) value (? '%))
  (:destructure (pm val percent) (declare (ignore pm)) (if percent `(- (/ ,val 100)) val)))
(defrule errval () (and value (? error))
  (:destructure (val err) (list val (if err err 0))))
(defrule conversion () (and '-> (* unit-factor))
  (:destructure (arrow unit-factors) (declare (ignore arrow)) unit-factors))
(defrule quantity () (and errval (* unit-factor) (? conversion)))

(defun read-quantity (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((expr (parse-list 'quantity (read stream t nil t))))
    (unless expr
      (error "Syntax error in quantity definition."))
    (destructuring-bind ((val err) unit-a unit-b) expr
      `(eval-quantity ,val ,err (list ,@unit-a) (list ,@unit-b)))))

(set-dispatch-macro-character #\# #\q #'read-quantity)

(defgeneric eval-quantity (value error unit-a unit-b))
;; #q(<n> [+/- <n>] [m / s])
(defmethod eval-quantity ((value number) (error number) unit-a (unit-b (eql nil)))
  (make-instance 'quantity :value value :error error :unit unit-a))
;; #q(<n> [+/- <n>] m / s -> km / h)
(defmethod eval-quantity ((value number) (error number) unit-a unit-b)
  (make-instance 'quantity :value (convert-units value unit-a unit-b) :error (if (minusp error) error (convert-units error unit-a unit-b)) :unit unit-b))
;; #q(<q>)
(defmethod eval-quantity ((value quantity) (error (eql 0)) (unit-a (eql nil)) (unit-b (eql nil)))
  (value value))
;; #q(<q> -> km / h)
(defmethod eval-quantity ((q quantity) (error (eql 0)) unit-a (unit-b (eql nil)))
  (convert-units q unit-a))
;; #q(<q> km / h)
(defmethod eval-quantity ((q quantity) (error (eql 0)) (unit-a (eql nil)) unit-b)
  (convert-units q unit-b))
