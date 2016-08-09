(in-package :pq)

;; Helper functions -----------------------------------------------------------------

(defun add-errors (&rest errors)
  (let ((sum (loop for err in errors sum (expt err 2))))
    (if (plusp sum) (sqrt sum) 0)))

(defun add-aerr (&rest quantities)
  (apply #'add-errors (loop for q in quantities collect (absolute-error q))))
(defun add-rerr (&rest quantities)
  (- (apply #'add-errors (loop for q in quantities collect (relative-error q)))))

(defmacro dup-quantity (q &key v e u)
  `(make-instance 'quantity
                  :value ,(if (or (null v) (eql q v)) `(value ,q) v)
                  :error ,(if (or (null e) (eql q e)) `(error-direct ,q) e)
                  :unit ,(if (or (null u) (eql q u)) `(copy-tree (unit ,q)) u)))

;; Actual functions -----------------------------------------------------------------

(defun quantityp (object)
  (eql (type-of object) 'quantity))

(defun copy-quantity (q)
  `(dup-quantity ,q))

(defgeneric binary+ (a b))
(defmethod binary+ ((a quantity) (b quantity))
  (let ((b2 (convert-units b (unit a))))
    (dup-quantity a :v (+ (value a) (value b2)) :e (add-aerr a b))))
(defun q+ (&rest numbers)
  ;; FIXME: single and no argument do not work
  (assert (l> numbers 1))
  (reduce #'binary+ numbers))

(defgeneric unary- (a))
(defmethod unary- ((a quantity))
  (dup-quantity a :v (- (value a))))
(defgeneric binary- (a b))
(defmethod binary- ((a quantity) (b quantity))
  (let ((b2 (convert-units b (unit a))))
    (dup-quantity a :v (- (value a) (value b2)) :e (add-aerr a b))))
(defun q- (&rest numbers)
  ;; FIXME no argument does not work
  (assert (l> numbers 0))
  (if (l= numbers 1)
      (unary- (first numbers))
      (reduce #'binary- numbers)))

(defgeneric binary* (a b))
(defmethod binary* ((a quantity) (b quantity))
  (dup-quantity a :v (* (value a) (value b)) :e (add-rerr a b) :u (multiply-units (unit a) (unit b))))
(defmethod binary* ((a number) (b quantity))
  (dup-quantity b :v (* a (value b)) :e (if (plusp (error-direct b)) (* a (error-direct b)) (error-direct b))))
(defmethod binary* ((a quantity) (b number))
  (binary* b a))
(defun q* (&rest numbers)
  ;; FIXME single and no argument do not work
  (assert (l> numbers 1))
  (reduce #'binary* numbers))

(defgeneric binary/ (a b))
(defmethod binary/ ((a quantity) (b quantity))
  (dup-quantity a :v (/ (value a) (value b)) :e (add-rerr a b) :u (divide-units (unit a) (unit b))))
(defmethod binary/ ((a number) (b quantity))
  (dup-quantity b :v (/ a (value b)) :e (if (plusp (error-direct b)) (* a (error-direct b) (expt (value b) -2)) (error-direct b)) :u (power-unit (unit b) -1)))
(defmethod binary/ ((a quantity) (b number))
  (dup-quantity a :v (/ (value a) b) :e (if (plusp (error-direct a)) (/ (error-direct a) b) (error-direct a))))
(defun q/ (&rest numbers)
  ;; FIXME single and no argument do not work
  (assert (l> numbers 1))
  (reduce #'binary/ numbers))

(defgeneric qpow (base power))
(defmethod qpow ((base quantity) (power integer))
  (cond
    ((plusp power) (dup-quantity base :v (expt (value base) power) :e (apply #'add-rerr (loop for i below power collect base)) :u (power-unit (unit base) power)))
))
