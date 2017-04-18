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

(defmacro with-unitless-quantity ((val err quantity &key error) &body body)
  (with-gensyms (expansion factor)
    `(multiple-value-bind (,expansion ,factor) (expand-unit (unit ,quantity))
       (when ,expansion
         (error "Quantity is required to be without unit!"))
       (let ((,val (* ,factor (value ,quantity)))
             (,err ,(case error
                          (:absolute `(* ,factor (aerr ,quantity)))
                          (:relative `(rerr ,quantity))
                          (t `(if (minusp (error-direct ,quantity)) (error-direct ,quantity) (* ,factor (error-direct ,quantity)))))))
         ,@body))))

;; Wrapper functions -----------------------------------------------------------------

(defun quantityp (object)
  (eql (type-of object) 'quantity))

(defun copy-quantity (q)
  `(dup-quantity ,q))

(defgeneric binary+ (a b))
(defmethod binary+ ((a quantity) (b quantity))
  (let ((b2 (convert-unit% b (unit a))))
    (dup-quantity a :v (+ (value a) (value b2)) :e (add-aerr a b))))
(defmethod binary+ ((a quantity) (b real))
  (with-unitless-quantity (val err a)
    (declare (ignore err))
    (dup-quantity a :v (+ val b))))
(defmethod binary+ ((a real) (b quantity))
  (with-unitless-quantity (val err b)
    (declare (ignore err))
    (dup-quantity b :v (+ val a))))
(defmethod binary+ ((a real) (b real))
  (+ a b))
(defun q+ (&rest quantities)
  (cond
    ((null quantities) 0)
    ((l= quantities 1) (car quantities))
    (t (reduce #'binary+ quantities))))
(export 'q+)

(defgeneric unary- (a))
(defmethod unary- ((a quantity))
  (dup-quantity a :v (- (value a))))
(defmethod unary- ((a real))
  (- a))
(defgeneric binary- (a b))
(defmethod binary- ((a quantity) (b quantity))
  (let ((b2 (convert-unit% b (unit a))))
    (dup-quantity a :v (- (value a) (value b2)) :e (add-aerr a b))))
(defmethod binary- ((a real) (b real))
  (- a b))
(defun q- (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary- number))
    (t (binary- number (apply #'q+ more-numbers)))))
(export 'q-)

(defgeneric binary* (a b))
(defmethod binary* ((a quantity) (b quantity))
  (dup-quantity a :v (* (value a) (value b)) :e (add-rerr a b) :u (multiply-units (unit a) (unit b))))
(defmethod binary* ((a real) (b quantity))
  (dup-quantity b :v (* a (value b)) :e (if (plusp (error-direct b)) (* (abs a) (error-direct b)) (error-direct b))))
(defmethod binary* ((a quantity) (b real))
  (binary* b a))
(defmethod binary* ((a real) (b real))
  (* a b))
(defun q* (&rest numbers)
  (cond
    ((null numbers) 1)
    ((l= numbers 1) (car numbers))
    (t (reduce #'binary* numbers))))
(export 'q*)

(defgeneric unary/ (a))
(defmethod unary/ ((a real))
  (/ a))
(defmethod unary/ ((a quantity))
  (make-quantity% :value (/ (value a)) :error (- (rerr a)) :unit (power-unit (unit a) -1)))
(defgeneric binary/ (a b))
(defmethod binary/ ((a quantity) (b quantity))
  (dup-quantity a :v (/ (value a) (value b)) :e (add-rerr a b) :u (divide-units (unit a) (unit b))))
(defmethod binary/ ((a real) (b quantity))
  (dup-quantity b :v (/ a (value b)) :e (if (plusp (error-direct b)) (* a (error-direct b) (expt (value b) -2)) (error-direct b)) :u (power-unit (unit b) -1)))
(defmethod binary/ ((a quantity) (b real))
  (dup-quantity a :v (/ (value a) b) :e (if (plusp (error-direct a)) (/ (error-direct a) b) (error-direct a))))
(defmethod binary/ ((a real) (b real))
  (/ a b))
(defun q/ (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary/ number))
    (t (binary/ number (apply #'q* more-numbers)))))
(export 'q/)

;; Power function (integer exponent, real result)
(defgeneric qpow (base power) (:documentation "Computes the power of a quantity. The power must be an integer."))
(defmethod qpow ((base real) (power integer))
  (expt base power))
(defmethod qpow ((base quantity) (power integer))
  (if (zerop power)
      (make-quantity% :value 1)
      (dup-quantity base
                    :v (expt (value base) power)
                    :e (if (/= 0 (value base)) (apply #'add-rerr (loop for i below (abs power) collect base)) 0)
                    :u (power-unit (unit base) power))))
(defmethod qpow ((base real) (power quantity))
  (with-unitless-quantity (val err power :error :absolute)
    (declare (ignore err))
    (when (has-error-p power)
      (error "Real cannot be raised to a power with uncertainty/error."))
    (unless (integerp (value power))
      (error "Use qexpt to raise to non-integer powers."))
    (let ((v (expt base val)))
      (make-quantity% :value v :error 0))))
(defmethod qpow ((base quantity) (power quantity))
  (with-unitless-quantity (pval perr power :error :absolute)
    (declare (ignore pval perr))
    (when (has-error-p power)
      (error "Quantity cannot be raised to a power with uncertainty/error."))
    (unless (integerp (value power))
      (error "Use pexpt to raise to non-integer powers."))
    (qpow base (value power))))
(export 'qpow)

;; Nth root (integer degree, real result)
(defgeneric qroot (radicand degree) (:documentation "Computes the nth root of a given quantity. The degree must be an integer and the result real."))
(defmethod qroot ((radicand real) (degree integer))
  (unless (plusp degree)
    (error "The degree of a root must be greater than zero."))
  (cond
    ((and (minusp radicand) (oddp degree)) (- (expt (- radicand) (/ degree))))
    ((and (minusp radicand) (evenp degree)) (error "Cannot compute the root of a negative number when the degree is even."))
    (t (expt radicand (/ degree)))))
(defmethod qroot ((radicand quantity) (degree integer))
  (unless (plusp degree)
    (error "The degree of a root must be greater than zero."))
  (let ((val (qroot (value radicand) degree)))
    (make-quantity% :value val :error (if (zerop (value radicand)) 0 (abs (/ (* val (aerr radicand)) (value radicand) degree))) :unit (root-unit (unit radicand) degree))))
(defmethod qroot ((radicand real) (degree quantity))
  (unless (unitlessp degree)
    (error "The degree of a root must be unitless."))
  (unless (errorlessp degree)
    (error "The degree of a root is not allowed to have uncertainty/error. Use expt instead."))
  (make-quantity% :value (qroot radicand (value degree))))
(defmethod qroot ((radicand quantity) (degree quantity))
  (unless (unitlessp degree)
    (error "The degree of a root must be unitless."))
  (unless (errorlessp degree)
    (error "The degree of a root is not allowed to have uncertainty/error. Use expt instead."))
  (qroot radicand (value degree)))
(export 'qroot)

;; Square root (real result)
(defun qsqrt (quantity)
  "Computes the square root of a given quantity. Result must always be real."
  (qroot quantity 2))
(export 'qsqrt)

;; Exponentiation, base e
(defgeneric qexp (exponent) (:documentation "Exponentiates e to the given quantity. The exponent must be real."))
(defmethod qexp ((exponent real))
  (exp exponent))
(defmethod qexp ((exponent quantity))
  (unless (unitlessp exponent)
    (error "Exponents must be unitless."))
  (let ((val (exp (value exponent))))
    (make-quantity% :value val :error (abs (* val (aerr exponent))))))

;; Exponentiation, base given
(defgeneric qexpt (base exponent) (:documentation "Exponentiates the given base. Base and exponent must be real."))
(defmethod qexpt ((base real) (exponent real))
  (expt base exponent))
(defmethod qexpt ((base real) (exponent quantity))
  (unless (unitlessp exponent)
    (error "Exponents must be unitless."))
  (when (and (minusp base) (has-error-p exponent))
    (error "Error propagation for exponentiation of a negative base with an exponent with uncertainty/error is undefined."))
  (let ((val (qexpt base (value exponent))))
    (make-quantity% :value val :error (abs (* (log base) val (aerr exponent))))))
(defmethod qexpt ((base quantity) (exponent integer))
  (qpow base exponent))
(defmethod qexpt ((base quantity) (exponent ratio))
  (let ((n (numerator exponent)) (d (denominator exponent)))
    (qroot (qpow base n) d)))
(defmethod qexpt ((base quantity) (exponent float))
  (unless (unitlessp base)
    (error "Exponentiation of a base with unit with a floating point exponent is undefined."))
  (let ((val (qexpt (value base) exponent)))
    (make-quantity% :value val :error (if (zerop (value base)) 0 (abs (/ (* exponent val (aerr base)) (value base)))))))
(defmethod qexpt ((base quantity) (exponent quantity))
  (unless (unitlessp exponent)
    (error "Exponents must be unitless."))
  (if (errorlessp exponent)
      (qexpt base (value exponent))
      ;; -> Exponent is unitless, but has an error. Base is anything.
      (progn
        (unless (unitlessp base)
          (error "Exponentiation of base with units and exponent with uncertainty/error is not possible."))
        ;; -> Exponent is unitless, but has an error. Base is unitless.
        (when (and (minusp (value base)) (has-error-p exponent))
          (error "Error propagation for exponentiation of a negative base with an exponent with uncertainty/error is undefined."))
        (let* ((b (value base)) (db (aerr base)) (e (value exponent)) (de (aerr exponent)) (val (qexpt b e)))
          (make-quantity% :value val :error (if (zerop b) 0 (* (abs val) (sqrt (+ (expt (/ (* e db) b) 2) (expt (* (log b) de) 2))))))))))

(defgeneric qln (number))
(defmethod qln ((number real))
  (log number))
(defmethod qln ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (log val) :error (abs (/ err val)))))
(export 'qln)

(defgeneric qlog (number base))
(defmethod qlog ((number real) (base real))
  (log number base))
(defmethod qlog ((number quantity) (base real))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (log val base) :error (abs (/ err val (log base))))))
(defmethod qlog ((number real) (base quantity))
  (with-unitless-quantity (b db base :error :absolute)
    (make-quantity% :value (log number b) :error (abs (/ (* (log number) db) b (expt (log b) 2))))))
(defmethod qlog ((number quantity) (base quantity))
  (with-unitless-quantity (n dn number :error :absolute)
    (with-unitless-quantity (b db base :error :absolute)
      (make-quantity% :value (log n b) :error (sqrt (+ (expt (/ dn n (log b)) 2) (expt (/ (* (log n) db) b (expt (log b) 2)) 2)))))))
(export 'qlog)

(defgeneric qsin (number))
(defmethod qsin ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (sin val) :error (abs (* (cos val) err)))))
(defmethod qsin ((number number))
  (sin number))
(export 'qsin)

(defgeneric qcos (number))
(defmethod qcos ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (cos val) :error (abs (* (sin val) err)))))
(defmethod qcos ((number number))
  (cos number))
(export 'qcos)

(defgeneric qtan (number))
(defmethod qtan ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (tan val) :error (abs (/ err (expt (cos val) 2))))))
(defmethod qtan ((number number))
  (tan number))
(export 'qtan)

(defgeneric qasin (number))
(defmethod qasin ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (asin val) :error (abs (/ err (sqrt (- 1 (expt val 2))))))))
(defmethod qasin ((number number))
  (asin number))
(export 'qasin)

(defgeneric qacos (number))
(defmethod qacos ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (acos val) :error (abs (/ err (sqrt (- 1 (expt val 2))))))))
(defmethod qacos ((number number))
  (acos number))
(export 'qacos)

(defgeneric qatan (number))
(defmethod qatan ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (atan val) :error (abs (/ err (+ 1 (expt val 2)))))))
(defmethod qatan ((number number))
  (atan number))
(export 'qatan)

(defgeneric qsinh (number))
(defmethod qsinh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (sinh val) :error (abs (* (cosh val) err)))))
(defmethod qsinh ((number number))
  (sinh number))
(export 'qsinh)

(defgeneric qcosh (number))
(defmethod qcosh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (cosh val) :error (abs (* (sinh val) err)))))
(defmethod qcosh ((number number))
  (cosh number))
(export 'qcosh)

(defgeneric qtanh (number))
(defmethod qtanh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (tanh val) :error (abs (* (- 1 (expt (tanh val) 2)) err)))))
(defmethod qtanh ((number number))
  (tanh number))
(export 'qtanh)

(defgeneric qasinh (number))
(defmethod qasinh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (asinh val) :error (abs (/ err (sqrt (+ (expt val 2) 1)))))))
(defmethod qasinh ((number number))
  (asinh number))
(export 'qasinh)

(defgeneric qacosh (number))
(defmethod qacosh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (acosh val) :error (abs (/ err (sqrt (- (expt val 2) 1)))))))
(defmethod qacosh ((number number))
  (acosh number))
(export 'qacosh)

(defgeneric qatanh (number))
(defmethod qatanh ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (atanh val) :error (abs (/ err (- 1 (expt val 2)))))))
(defmethod qatanh ((number number))
  (atanh number))
(export 'qatanh)

(defgeneric qabs (number))
(defmethod qabs ((number quantity))
  (dup-quantity number :v (abs (value number))))
(defmethod qabs ((number number))
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
