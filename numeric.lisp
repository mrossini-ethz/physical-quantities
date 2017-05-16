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
  `(make-quantity%
    :value ,(if (or (null v) (eql q v)) `(value ,q) v)
    :error ,(if (or (null e) (eql q e)) `(error-direct ,q) e)
    :unit ,(if (or (null u) (eql q u)) `(copy-tree (unit ,q)) u)))

(defmacro with-quantity-slots ((quantity val err) &body body)
  (with-gensyms (quant)
    `(let* ((,quant ,quantity) (,val (value ,quant)) (,err (aerr ,quant)))
       ,@body)))

;; Wrapper functions -----------------------------------------------------------------

(defun copy-quantity (q)
  `(dup-quantity ,q))

;; Operation definition macro --------------------------------------------------------

(defmacro defqop (name arg-list &body body)
  `(defun ,name ,arg-list
     ,@(loop for arg in arg-list collect
            `(cond
               ((numberp ,arg) (setf ,arg (make-quantity :value ,arg)))
               ((not (quantityp ,arg)) (f-error operation-undefined-error () "Operation  (~a ~{~a~^ ~} is undefined if ~a is of type ~a." ',name ',arg-list ',arg (type-of ',arg)))))
     ,@body))

(defmacro error-propagation (&rest var-derivative-pairs)
  (unless (evenp (list-length var-derivative-pairs))
    (error "Invalid error propagation argument list."))
  (let* ((n (/ (list-length var-derivative-pairs) 2))
         (gensyms (loop repeat n collect (gensym))))
    `(let (,@(loop for i below n for var = (nth (* i 2) var-derivative-pairs) collect `(,(nth i gensyms) (value ,var))))
       (declare (ignorable ,@gensyms))
       (sqrt (+ ,@(loop
                     for i below n
                     for var = (nth (* i 2) var-derivative-pairs)
                     for exp = (nth (1+ (* i 2)) var-derivative-pairs)
                     collect `(expt (* (symbol-macrolet (,@(loop for j below n
                                                              for var2 = (nth (* j 2) var-derivative-pairs)
                                                              for gensym = (nth j gensyms)
                                                              collect `(,var2 ,gensym)))
                                         ,exp)
                                       (aerr ,var))
                                    2)))))))

;; Operations ------------------------------------------------------------------------

;; Q+
(defqop binary+ (a b)
  (let ((b (convert-unit% b (unit a))))
    (make-quantity% :value (+ (value a) (value b)) :error (error-propagation a 1 b 1) :unit (unit a))))
(defun q+ (&rest quantities)
  (cond
    ((null quantities) (make-quantity% :value 0))
    ((l= quantities 1) (if (quantityp (first quantities)) (first quantities) (make-quantity% :value (first quantities))))
    (t (reduce #'binary+ quantities))))
(export 'q+)

;; Q-
(defqop unary- (a)
  (dup-quantity a :v (- (value a))))
(defqop binary- (a b)
  (let ((b (convert-unit% b (unit a))))
    (make-quantity% :value (- (value a) (value b)) :error (error-propagation a 1 b 1) :unit (unit a))))
(defun q- (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary- number))
    (t (binary- number (apply #'q+ more-numbers)))))
(export 'q-)

;; Q*
(defqop binary* (a b)
  (make-quantity% :value (* (value a) (value b)) :error (error-propagation a b b a) :unit (multiply-units (unit a) (unit b))))
(defun q* (&rest numbers)
  (cond
    ((null numbers) (make-quantity% :value 1))
    ((l= numbers 1) (if (quantityp (first numbers)) (dup-quantity (first numbers)) (make-quantity% :value (car numbers))))
    (t (reduce #'binary* numbers))))
(export 'q*)

;; Q/
(defqop unary/ (a)
  (make-quantity% :value (/ (value a)) :error (error-propagation a (expt a -2)) :unit (power-unit (unit a) -1)))
(defqop binary/ (a b)
  (make-quantity% :value (/ (value a) (value b)) :error (error-propagation a (/ b) b (/ a b b)) :unit (divide-units (unit a) (unit b))))
(defun q/ (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary/ number))
    (t (binary/ number (apply #'q* more-numbers)))))
(export 'q/)

;; QPOW: Power function (integer exponent, real result)
(defqop qpow (base power)
  (unless (unitlessp power)
    (f-error invalid-unit-operation-error () "POWER in operation (QPOW BASE POWER) must be unitless."))
  (unless (errorlessp power)
    (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER is quantity with uncertainty/error."))
  (unless (integerp (value power))
    (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER has non-integer value."))
  (make-quantity% :value (expt (value base) (value power)) :error (error-propagation base (if (zerop power) 0 (* power (expt base (1- power)))) power 0) :unit (power-unit (unit base) (value power))))
(export 'qpow)

;; QROOT: Nth root (integer degree, real result)
(defqop qroot (radicand degree)
  (unless (unitlessp degree)
    (f-error invalid-unit-operation-error () "DEGREE in operation (QROOT RADICAND DEGREE) must be unitless."))
  (unless (errorlessp degree)
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) is undefined if DEGREE is quantity with uncertainty/error."))
  (unless (plusp (value degree))
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined unless DEGREE is positive."))
  (when (and (minusp (value radicand)) (evenp (value degree)))
     (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined if RADICAND has negative value and DEGREE is even."))
  (handler-case (multiple-value-bind (unit conv) (root-unit (unit radicand) (value degree))
                  (if (and (minusp (value radicand)) (oddp (value degree)))
                      (let ((val (* conv (- (expt (- (value radicand)) (/ (value degree)))))))
                        (make-quantity% :value val :error (error-propagation radicand (if (zerop radicand) 0 (/ val radicand degree)) degree 0) :unit unit))
                      (let ((val (* conv (expt (value radicand) (/ (value degree))))))
                        (make-quantity% :value val :error (error-propagation radicand (if (zerop radicand) 0 (/ val radicand degree)) degree 0) :unit unit))))
    (operation-undefined-error () (f-error invalid-unit-operation-error () "The unit of RADICAND in operation (QROOT RADICAND DEGREE) must have a power ~
                                                                            that is a multiple of DEGREE." degree (str-unit (unit radicand))))))
(export 'qroot)

;; QSQRT: Square root (real result)
(defun qsqrt (quantity)
  "Computes the square root of a given quantity. Result must always be real."
  (qroot quantity 2))
(export 'qsqrt)

;; QEXP: Exponentiation, base e
(defqop qexp (exponent)
  (unless (unitlessp exponent)
    (f-error invalid-unit-operation-error () "EXPONENT in the operation (QEXP EXPONENT) must be unitless."))
  (let ((val (exp (value exponent))))
    (make-quantity% :value val :error (abs (* val (aerr exponent))))))
(export 'qexp)

;; Exponentiation, base given
(defqop qexpt (base exponent)
  (unless (unitlessp exponent)
    (f-error invalid-unit-operation-error () "EXPONENT in the operation (QEXPT BASE EXPONENT) must be unitless."))
  (when (and (has-error-p exponent) (has-unit-p base))
    (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT is quantity with uncertainty/error."))
  (when (and (has-unit-p base) (floatp (value exponent)))
    (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT has a floating point value."))
  (when (and (minusp (value base)) (has-error-p exponent))
    (f-error error-propagation-error () "Error propagation undefined for operation (QEXPT BASE EXPONENT) if BASE has negative value and EXPONENT is quantity with uncertainty/error."))
  (cond
    ((and (integerp (value exponent)) (errorlessp exponent)) (qpow base exponent))
    ((and (typep (value exponent) 'ratio) (errorlessp exponent)) (qroot (qpow base (numerator (value exponent))) (denominator (value exponent))))
    (t (let ((val (expt (value base) (value exponent))))
         (make-quantity% :value val :error (error-propagation base (if (zerop base) 0 (* exponent (expt base (1- exponent)))) exponent (if (zerop base) 0 (/ val (log base)))))))))
(export 'qexpt)

(defqop qln (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QLN NUMBER) must be unitless."))
  (make-quantity% :value (log (value number)) :error (error-propagation number (/ number))))
(export 'qln)

(defqop qlog (number base)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QLOG NUMBER BASE) must be unitless."))
  (unless (unitlessp base)
    (f-error invalid-unit-operation-error () "BASE in operation (QLOG NUMBER BASE) must be unitless."))
  (make-quantity% :value (log (value number) (value base)) :error (error-propagation number (/ 1 number (log base)) base (/ (log number) base (expt (log base) 2)))))
(export 'qlog)

(defqop qsin (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QSIN NUMBER) must be unitless."))
  (make-quantity% :value (sin (value number)) :error (error-propagation number (cos number))))
(export 'qsin)

(defqop qcos (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QCOS NUMBER) must be unitless."))
  (make-quantity% :value (cos (value number)) :error (error-propagation number (sin number))))
(export 'qcos)

(defqop qtan (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QTAN NUMBER) must be unitless."))
  (make-quantity% :value (tan (value number)) :error (error-propagation number (/ (expt (cos number) 2)))))
(export 'qtan)

(defqop qasin (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QASIN NUMBER) must be unitless."))
  (make-quantity% :value (asin (value number)) :error (error-propagation number (/ (sqrt (- 1 (expt number 2)))))))
(export 'qasin)

(defqop qacos (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QACOS NUMBER) must be unitless."))
  (make-quantity% :value (acos (value number)) :error (error-propagation number (/ -1 (sqrt (- 1 (expt number 2)))))))
(export 'qacos)

(defqop qatan (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QATAN NUMBER) must be unitless."))
  (make-quantity% :value (atan (value number)) :error (error-propagation number (/ (1+ (expt number 2))))))
(export 'qatan)

(defqop qsinh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QSINH NUMBER) must be unitless."))
  (make-quantity% :value (sinh (value number)) :error (error-propagation number (cosh number))))
(export 'qsinh)

(defqop qcosh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QCOSH NUMBER) must be unitless."))
  (make-quantity% :value (cosh (value number)) :error (error-propagation number (sinh number))))
(export 'qcosh)

(defqop qtanh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QTANH NUMBER) must be unitless."))
  (make-quantity% :value (tanh (value number)) :error (error-propagation number (/ (expt (cosh number) 2)))))
(export 'qtanh)

(defqop qasinh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QASINH NUMBER) must be unitless."))
  (make-quantity% :value (asinh (value number)) :error (error-propagation number (/ (sqrt (1+ (expt number 2)))))))
(export 'qasinh)

(defqop qacosh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QACOSH NUMBER) must be unitless."))
  (make-quantity% :value (acosh (value number)) :error (error-propagation number (/ 1 (sqrt (1+ number)) (sqrt (1- number))))))
(export 'qacosh)

(defqop qatanh (number)
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QATANH NUMBER) must be unitless."))
  (make-quantity% :value (atanh (value number)) :error (error-propagation number (/ (- 1 (expt number 2))))))
(export 'qatanh)

(defgeneric qabs (number))
(defmethod qabs ((number quantity))
  (dup-quantity number :v (abs (value number))))
(defmethod qabs ((number real))
  (abs number))
(export 'qabs)

;; Q-OP macros and functions --------------------------------------------------------

(defun q-op-insert (tree)
  ;; Very simple code walker that replaces numeric operations like + with q+ etc.
  (let ((tree (macroexpand tree)))
    (macrolet ((qcase ((form) &rest symbols) `(case ,form ,@(loop for s in symbols collect `(,s ',(symb "Q" s))) (t ,form))))
      `(,@(loop for leaf in tree
             for n upfrom 0 collect
               (cond
                 ((listp leaf) (q-op-insert leaf))
                 ((zerop n) (qcase (leaf) + - * / exp expt log sqrt sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh))
                 (t leaf)))))))

(defmacro qop (qform)
  (funcall #'q-op-insert qform))
(export 'qop)

;; Predicates/Tests -----------------------------------------------------------------

(defgeneric qequal (x y) (:documentation "Determines whether two quantities are equal in value, error and unit."))
(defmethod qequal ((x real) (y real))
  (equal x y))
(defmethod qequal ((x quantity) (y real))
  (and (errorlessp x) (unitlessp x) (equal (value x) y)))
(defmethod qequal ((x real) (y quantity))
  (and (errorlessp y) (unitlessp y) (equal x (value y))))
(defmethod qequal ((x quantity) (y quantity))
  (and (equal (value x) (value y)) (equal (error-direct x) (error-direct y)) (units-equal (unit x) (unit y))))
(export 'qequal)

(defgeneric qequalp (x y) (:documentation "Determines whether two quantities are equalp in value, error and unit."))
(defmethod qequalp ((x real) (y real))
  (equalp x y))
(defmethod qequalp ((x quantity) (y real))
  (and (errorlessp x) (unitlessp x) (equalp (value x) y)))
(defmethod qequalp ((x real) (y quantity))
  (qequalp y x))
(defmethod qequalp ((x quantity) (y quantity))
  (and (equalp (value x) (value y))
       (or (equalp (error-direct x) (error-direct y))
           (equalp (aerr x) (aerr y))
           (equalp (rerr x) (rerr y)))
       (units-equalp (unit x) (unit y))))
(export 'qequalp)

(defgeneric q= (x y &optional p-value) (:documentation "Determines whether the value of two quantities are equal."))
(defmethod q= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (= x y))
(defmethod q= ((x quantity) (y real) &optional (p-value 95/100))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q=." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (= (value x) y)
      ;; Quantity has uncertainty
      (let* ((max-delta (* (aerr x) (confidence-interval p-value :two-sided t))) (delta (abs (- (value x) y))))
        ;; delta: the difference between the two values
        ;; max-delta: the range within which delta is acceptable
        (< delta max-delta))))
(defmethod q= ((x real) (y quantity) &optional (p-value 95/100))
  (q= y x p-value))
(defmethod q= ((x quantity) (y quantity) &optional (p-value 95/100))
  ;; Calculate the difference between the quantities. This may raise an error.
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q=." (str-unit x) (str-unit y)))
  (let ((val (q- y x)))
    (if (errorlessp val)
        ;; Difference has no uncertainty
        (= (value val) 0)
        ;; Difference has uncertainty
        (let ((confidence-interval (* (aerr val) (confidence-interval p-value :two-sided t))))
          (< (abs (value val)) confidence-interval)))))
(export 'q=)

(defun q/= (x y &optional (p-value 0.95))
  (not (q= x y p-value)))
(export 'q/=)

(defgeneric q< (x y &optional p-value) (:documentation "Determines whether the one quantity is less than another quantity"))
(defmethod q< ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (< x y))
(defmethod q< ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (< (value x) y)
      ;; Quantity has uncertainty
      (< (+ (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q< ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (< x (value y))
      ;; Quantity has uncertainty
      (< x (- (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q< ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<." (str-unit x) (str-unit y)))
  (let ((delta (q- y x)))
    (> (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q<)

(defgeneric q<= (x y &optional p-value) (:documentation "Determines whether the one quantity is less or equal to another quantity"))
(defmethod q<= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (<= x y))
(defmethod q<= ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (<= (value x) y)
      ;; Quantity has uncertainty
      (<= (+ (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q<= ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (<= x (value y))
      ;; Quantity has uncertainty
      (<= x (- (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q<= ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q<=." (str-unit x) (str-unit y)))
  (let ((delta (q- y x)))
    (>= (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q<=)

(defgeneric q> (x y &optional p-value) (:documentation "Determines whether the one quantity is greater than another quantity"))
(defmethod q> ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (> x y))
(defmethod q> ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (> (value x) y)
      ;; Quantity has uncertainty
      (> (- (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q> ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (> x (value y))
      ;; Quantity has uncertainty
      (> x (+ (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q> ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>." (str-unit x) (str-unit y)))
  (let ((delta (q- x y)))
    (> (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q>)

(defgeneric q>= (x y &optional p-value) (:documentation "Determines whether the one quantity is greater or equal to another quantity"))
(defmethod q>= ((x real) (y real) &optional p-value)
  (declare (ignore p-value))
  (>= x y))
(defmethod q>= ((x quantity) (y real) &optional (p-value 0.95))
  (unless (unitlessp x)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp x)
      ;; Quantity has no uncertainty
      (>= (value x) y)
      ;; Quantity has uncertainty
      (>= (- (value x) (* (aerr x) (confidence-interval p-value :two-sided nil))) y)))
(defmethod q>= ((x real) (y quantity) &optional (p-value 0.95))
  (unless (unitlessp y)
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit x) (str-unit y)))
  ;; Quantity is unitless
  (if (errorlessp y)
      ;; Quantity has no uncertainty
      (>= x (value y))
      ;; Quantity has uncertainty
      (>= x (+ (value y) (* (aerr y) (confidence-interval p-value :two-sided nil))))))
(defmethod q>= ((x quantity) (y quantity) &optional (p-value 0.95))
  (unless (units-convertible (unit x) (unit y))
    (f-error invalid-unit-conversion-error () "The units ~a and ~a are incompatible under operation Q>=." (str-unit x) (str-unit y)))
  (let ((delta (q- x y)))
    (>= (value delta) (* (aerr delta) (confidence-interval p-value :two-sided nil)))))
(export 'q>=)

;; Rounding functions ---------------------------------------------------------------

(defun round-to (number digits &optional place)
  (unless place
    (setf place (floor (log number 10))))
  (* (round number (expt 10 (- place (- digits 1)))) (expt 10 (- place (1- digits)))))

(defun qround (quantity &key digits place)
  ;; Rounding rule from Review of Particle Physics, Introduction, Section 5.3 by the Particle Data Group (http://pdg.lbl.gov):
  ;; "The basic rule states that if the three highest order
  ;; digits of the error lie between 100 and 354, we round to
  ;; two significant digits. If they lie between 355 and 949, we
  ;; round to one significant digit. Finally, if they lie between
  ;; 950 and 999, we round up to 1000 and keep two significant
  ;; digits. In all cases, the central value is given with a precision
  ;; that matches that of the error. So, for example, the result
  ;; (coming from an average) 0.827 +/- 0.119 would appear as
  ;; 0.83 +/- 0.12, while 0.827 +/- 0.367 would turn into 0.8 +/- 0.4."
  (cond
    ;; Neither digits nor place are specified
    ((and (not place) (not digits))
     ;; Round to the nearest integer
     (setf place 0)
     (setf digits 1))
    ;; Place is not specified
    ((not place)
     ;; Determine the place from either the error or the value of the quantity
     (setf place (floor (log (if (plusp (error-direct quantity)) (aerr quantity) (value quantity)) 10))))
    ;; Digits are not specified
    ((not digits)
     (cond
       ;; Determine digits through error
       ((plusp (error-direct quantity))
        (let ((test (truncate (/ (aerr quantity) (expt 10 (- place 2))))))
          (setf digits (if (or (<= test 354) (>= test 950)) 2 1))
          (when (>= test 950)
            (incf place))))
       ;; Set digits to 1
       (t (setf digits 1)))))
  (values (dup-quantity quantity :v (round-to (value quantity) digits place) :e (round-to (aerr quantity) digits place)) digits place))
(export 'qround)
