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
(defmethod binary+ ((a quantity) (b number))
  (with-unitless-quantity (val err a)
    (declare (ignore err))
    (dup-quantity a :v (+ val b))))
(defmethod binary+ ((a number) (b quantity))
  (with-unitless-quantity (val err b)
    (declare (ignore err))
    (dup-quantity b :v (+ val a))))
(defmethod binary+ ((a number) (b number))
  (+ a b))
(defun q+ (&rest numbers)
  (cond
    ((null numbers) 0)
    ((l= numbers 1) (car numbers))
    (t (reduce #'binary+ numbers))))
(export 'q+)

(defgeneric unary- (a))
(defmethod unary- ((a quantity))
  (dup-quantity a :v (- (value a))))
(defmethod unary- ((a number))
  (- a))
(defgeneric binary- (a b))
(defmethod binary- ((a quantity) (b quantity))
  (let ((b2 (convert-unit% b (unit a))))
    (dup-quantity a :v (- (value a) (value b2)) :e (add-aerr a b))))
(defmethod binary- ((a number) (b number))
  (- a b))
(defun q- (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary- number))
    (t (binary- number (apply #'q+ more-numbers)))))
(export 'q-)

(defgeneric binary* (a b))
(defmethod binary* ((a quantity) (b quantity))
  (dup-quantity a :v (* (value a) (value b)) :e (add-rerr a b) :u (multiply-units (unit a) (unit b))))
(defmethod binary* ((a number) (b quantity))
  (dup-quantity b :v (* a (value b)) :e (if (plusp (error-direct b)) (* a (error-direct b)) (error-direct b))))
(defmethod binary* ((a quantity) (b number))
  (binary* b a))
(defmethod binary* ((a number) (b number))
  (* a b))
(defun q* (&rest numbers)
  (cond
    ((null numbers) 1)
    ((l= numbers 1) (car numbers))
    (t (reduce #'binary* numbers))))
(export 'q*)

(defgeneric unary/ (a))
(defmethod unary- ((a number))
  (/ a))
(defmethod unary- ((a quantity))
  (make-quantity% :value (/ (value a)) :error (- (rerr a)) :unit (power-unit (unit a) -1)))
(defgeneric binary/ (a b))
(defmethod binary/ ((a quantity) (b quantity))
  (dup-quantity a :v (/ (value a) (value b)) :e (add-rerr a b) :u (divide-units (unit a) (unit b))))
(defmethod binary/ ((a number) (b quantity))
  (dup-quantity b :v (/ a (value b)) :e (if (plusp (error-direct b)) (* a (error-direct b) (expt (value b) -2)) (error-direct b)) :u (power-unit (unit b) -1)))
(defmethod binary/ ((a quantity) (b number))
  (dup-quantity a :v (/ (value a) b) :e (if (plusp (error-direct a)) (/ (error-direct a) b) (error-direct a))))
(defmethod binary/ ((a number) (b number))
  (/ a b))
(defun q/ (number &rest more-numbers)
  (cond
    ((null more-numbers) (unary/ number))
    (t (binary/ number (apply #'q* more-numbers)))))
(export 'q/)

(defgeneric qpow (base power))
(defmethod qpow ((base quantity) (power integer))
  (cond
    ((zerop power) (if (/= 0 (value base))
                       (make-quantity% :value 1)
                       (error "Zero raised to the power of zero!")))
    (t (dup-quantity base :v (expt (value base) power) :e (apply #'add-rerr (loop for i below (abs power) collect base)) :u (power-unit (unit base) power)))))
(defmethod qpow ((base number) (power quantity))
  (with-unitless-quantity (val err power :error :absolute)
    (let ((v (expt base val)))
      (make-quantity% :value v :error (abs (* v (log base) err))))))
(defmethod qpow ((base quantity) (power quantity))
  (with-unitless-quantity (bval berr base :error :absolute)
    (with-unitless-quantity (pval perr power :error :absolute)
      (when (not (integerp pval))
        (error "Cannot raise quantity to a non-integer power!"))
      (cond
        ((zerop pval) (if (/= 0 bval)
                          (make-quantity% :value 1)
                          (error "Zero raised to the power of zero!")))
        (t (let ((v (expt bval pval)))
             (make-quantity% :value v :error (* (abs v) (sqrt (+ (expt (* (/ pval bval) berr) 2) (expt (* (log bval) perr) 2)))) :unit (power-unit (unit base) pval))))))))
(export 'qpow)

(defgeneric qroot (radicand index))
(defmethod qroot ((radicand quantity) (index integer))
  (when (<= index 0)
    (error "Root index must be greater than zero!"))
  (make-quantity% :value (expt (value radicand) (/ index)) :error (- (/ (rerr radicand) index)) :unit (root-unit (unit radicand) index)))
(export 'qroot)

(defgeneric qexpt (base power))
(defmethod qexpt ((base quantity) (power quantity))
  (unless (rationalp (value power))
    (error "Exponent must be a rational quantity!"))
  (let ((n (numerator (value power))) (d (denominator (value power))))
    (qroot (qpow base n) d)))
(defmethod qexpt ((base quantity) (power rational))
  (let ((n (numerator power)) (d (denominator power)))
    (qroot (qpow base n) d)))
(defmethod qexpt ((base number) (power quantity))
  (qpow base power))
(defmethod qexpt ((base number) (power number))
  (expt base power))

(defgeneric qexp (quantity))
(defmethod qexp ((q quantity))
  (with-unitless-quantity (val err q :error :absolute)
    (make-quantity% :value (exp val) :error (* (exp val) err))))
(export 'qexp)

(defgeneric qln (number))
(defmethod qln ((number quantity))
  (with-unitless-quantity (val err number :error :absolute)
    (make-quantity% :value (log val) :error (abs (/ err val)))))
(export 'qln)

(defgeneric qlog (number base))
(defmethod qlog ((q quantity) (base number))
  (with-unitless-quantity (val err base :error :absolute)
    (make-quantity% :value (log val base) :error (abs (/ err val (log base))))))
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
