(in-package :pq)

;; Helper functions -----------------------------------------------------------------

(defmacro dup-quantity (q &key v e u)
  `(make-quantity%
    :value ,(if (or (null v) (eql q v)) `(value ,q) v)
    :error ,(if (or (null e) (eql q e)) `(error-direct ,q) e)
    :unit ,(if (or (null u) (eql q u)) `(copy-tree (unit ,q)) u)))

;; Operation definition macro --------------------------------------------------------

(defmacro defqop (name arg-list &body body)
  (flet ((decode-arg (arg)
           (let* ((str (symbol-name arg)) (len (length str)))
             (if (and (> len 2) (string-equal str "!u" :start1 (- len 2)))
                 (values (intern (subseq str 0 (- len 2))) t)
                 arg))))
    (let ((lambda-list (mapcar #'decode-arg arg-list)))
      `(defun ,name ,lambda-list
         ,@(loop for arg in arg-list collect
                (multiple-value-bind (symbol unitless) (decode-arg arg)
                  `(cond
                     ((numberp ,symbol)
                      (setf ,symbol (make-quantity :value ,symbol)))
                     ((not (quantityp ,symbol))
                      (f-error operation-undefined-error () "Operation  (~a ~{~a~^ ~}) is undefined if ~a is of type ~a." ',name ',lambda-list ',symbol (type-of ',symbol)))
                     ,@(when unitless
                         `(((has-unit-p ,symbol)
                            (restart-case (f-error invalid-unit-operation-error () "~a in operation (~a ~{~a~^ ~}) must be unitless." ',symbol ',name ',lambda-list)
                              (drop-unit () :report ,(format nil "Drop the unit from ~a." symbol) (setf ,symbol (make-quantity% :value (value ,symbol) :error (error-direct ,symbol)))))))))))
         ,@body))))
(export 'defqop)

;; Error propagation macro -----------------------------------------------------------

(defmacro error-propagation (&rest var-derivative-pairs)
  ;; Get the number of arguments
  (let ((n (list-length var-derivative-pairs)))
    (unless (evenp n)
      (error "Invalid error propagation argument list."))
    ;; Put variables and derivative-expressions into separate lists
    (destructuring-bind (variables expressions) (unzip var-derivative-pairs)
      ;; Generate a gensym for each variable
      (let ((gensyms (loop repeat (/ n 2) collect (gensym))))
        ;; Assign the values of each variable to a gensym
        `(let (,@(loop for var in variables for gensym in gensyms collect `(,gensym (value ,var))))
           (declare (ignorable ,@gensyms))
           ;; Loop over the variables and collect the error expression (assign a symbol-macro for each variable to avoid having to call (value ...) all the time)
           ,(let ((parts (loop for var in variables for expr in expressions collect
                              `(* (symbol-macrolet (,@(loop for var2 in variables for gensym in gensyms collect `(,var2 ,gensym))) ,expr) (aerr ,var)))))
              ;; Compute final error
              (if (> n 2)
                  `(sqrt (+ ,@(loop for p in parts collect `(expt ,p 2))))
                  `(abs ,@parts))))))))

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
(defqop qpow (base power!u)
  (unless (errorlessp power)
    (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER is quantity with uncertainty/error."))
  (unless (integerp (value power))
    (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER has non-integer value."))
  (make-quantity% :value (expt (value base) (value power)) :error (error-propagation base (if (zerop power) 0 (* power (expt base (1- power)))) power 0) :unit (power-unit (unit base) (value power))))
(export 'qpow)

;; QROOT: Nth root (integer degree, real result)
(defqop qroot (radicand degree!u)
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
(defqop qexp (exponent!u)
  (let ((val (exp (value exponent))))
    (make-quantity% :value val :error (abs (* val (aerr exponent))))))
(export 'qexp)

;; Exponentiation, base given
(defqop qexpt (base exponent!u)
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

(defqop qln (number!u)
  (make-quantity% :value (log (value number)) :error (error-propagation number (/ number))))
(export 'qln)

(defqop qlog (number!u base!u)
  (make-quantity% :value (log (value number) (value base)) :error (error-propagation number (/ 1 number (log base)) base (/ (log number) base (expt (log base) 2)))))
(export 'qlog)

(defqop qsin (number!u)
  (make-quantity% :value (sin (value number)) :error (error-propagation number (cos number))))
(export 'qsin)

(defqop qcos (number!u)
  (make-quantity% :value (cos (value number)) :error (error-propagation number (sin number))))
(export 'qcos)

(defqop qtan (number!u)
  (make-quantity% :value (tan (value number)) :error (error-propagation number (/ (expt (cos number) 2)))))
(export 'qtan)

(defqop qasin (number!u)
  (make-quantity% :value (asin (value number)) :error (error-propagation number (/ (sqrt (- 1 (expt number 2)))))))
(export 'qasin)

(defqop qacos (number!u)
  (make-quantity% :value (acos (value number)) :error (error-propagation number (/ -1 (sqrt (- 1 (expt number 2)))))))
(export 'qacos)

(defqop qatan (number!u)
  (make-quantity% :value (atan (value number)) :error (error-propagation number (/ (1+ (expt number 2))))))
(export 'qatan)

(defqop qsinh (number!u)
  (make-quantity% :value (sinh (value number)) :error (error-propagation number (cosh number))))
(export 'qsinh)

(defqop qcosh (number!u)
  (make-quantity% :value (cosh (value number)) :error (error-propagation number (sinh number))))
(export 'qcosh)

(defqop qtanh (number!u)
  (make-quantity% :value (tanh (value number)) :error (error-propagation number (/ (expt (cosh number) 2)))))
(export 'qtanh)

(defqop qasinh (number!u)
  (make-quantity% :value (asinh (value number)) :error (error-propagation number (/ (sqrt (1+ (expt number 2)))))))
(export 'qasinh)

(defqop qacosh (number!u)
  (make-quantity% :value (acosh (value number)) :error (error-propagation number (/ 1 (sqrt (1+ number)) (sqrt (1- number))))))
(export 'qacosh)

(defqop qatanh (number!u)
  (make-quantity% :value (atanh (value number)) :error (error-propagation number (/ (- 1 (expt number 2))))))
(export 'qatanh)

(defqop qabs (number)
  (make-quantity% :value (abs (value number)) :error (error-direct number) :unit (unit number)))
(export 'qabs)

;; Predicates/Tests -----------------------------------------------------------------

(defqop qequal (x y)
  (and (equal (value x) (value y)) (equal (error-direct x) (error-direct y)) (units-equal (unit x) (unit y))))
(export 'qequal)

(defqop qequalp (x y)
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
