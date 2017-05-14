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
  (unless (unitlessp power)
    (f-error invalid-unit-operation-error () "POWER in operation (QPOW BASE POWER) must be unitless."))
  (with-quantity-slots (power val err)
    (declare (ignore err))
    (when (has-error-p power)
      (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER is quantity with uncertainty/error."))
    (unless (integerp (value power))
      (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER has non-integer value."))
    (let ((v (expt base val)))
      (make-quantity% :value v :error 0))))
(defmethod qpow ((base quantity) (power quantity))
  (unless (unitlessp power)
    (f-error invalid-unit-operation-error () "POWER in operation (QPOW BASE POWER) must be unitless."))
  (with-quantity-slots (power pval perr)
    (declare (ignore pval perr))
    (when (has-error-p power)
      (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER is quantity with uncertainty/error."))
    (unless (integerp (value power))
      (f-error operation-undefined-error () "Operation (QPOW BASE POWER) undefined if POWER has non-integer value."))
    (qpow base (value power))))
(export 'qpow)

;; Nth root (integer degree, real result)
(defgeneric qroot (radicand degree) (:documentation "Computes the nth root of a given quantity. The degree must be an integer and the result real."))
(defmethod qroot ((radicand real) (degree integer))
  (unless (plusp degree)
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined unless DEGREE is positive."))
  (cond
    ((and (minusp radicand) (oddp degree)) (- (expt (- radicand) (/ degree))))
    ((and (minusp radicand) (evenp degree)) (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined if RADICAND has negative value and DEGREE is even."))
    (t (expt radicand (/ degree)))))
(defmethod qroot ((radicand quantity) (degree integer))
  (unless (plusp degree)
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) undefined unless DEGREE is positive."))
  (let ((val (qroot (value radicand) degree)))
    (handler-case (multiple-value-bind (unit conv) (root-unit (unit radicand) degree)
                    (make-quantity% :value (* val conv) :error (if (zerop (value radicand)) 0 (abs (/ (* val conv (aerr radicand)) (value radicand) (expt conv degree) degree))) :unit unit))
      (operation-undefined-error () (f-error invalid-unit-operation-error () "The unit of RADICAND in operation (QROOT RADICAND DEGREE) must have a power ~
                                                                              that is a multiple of DEGREE."
                                             degree (str-unit (unit radicand)))))))
(defmethod qroot ((radicand real) (degree quantity))
  (unless (unitlessp degree)
    (f-error invalid-unit-operation-error () "DEGREE in operation (QROOT RADICAND DEGREE) must be unitless."))
  (unless (errorlessp degree)
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) is undefined if DEGREE is quantity with uncertainty/error."))
  (make-quantity% :value (qroot radicand (value degree))))
(defmethod qroot ((radicand quantity) (degree quantity))
  (unless (unitlessp degree)
    (f-error invalid-unit-operation-error () "DEGREE in operation (QROOT RADICAND DEGREE) must be unitless."))
  (unless (errorlessp degree)
    (f-error operation-undefined-error () "Operation (QROOT RADICAND DEGREE) is undefined if DEGREE is quantity with uncertainty/error."))
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
    (f-error invalid-unit-operation-error () "EXPONENT in the operation (QEXP EXPONENT) must be unitless."))
  (let ((val (exp (value exponent))))
    (make-quantity% :value val :error (abs (* val (aerr exponent))))))

;; Exponentiation, base given
(defgeneric qexpt (base exponent) (:documentation "Exponentiates the given base. Base and exponent must be real."))
(defmethod qexpt ((base real) (exponent real))
  (expt base exponent))
(defmethod qexpt ((base real) (exponent quantity))
  (unless (unitlessp exponent)
    (f-error invalid-unit-operation-error () "EXPONENT in the operation (QEXPT BASE EXPONENT) must be unitless."))
  (when (and (minusp base) (has-error-p exponent))
    (f-error error-propagation-error () "Error propagation undefined for operation (QEXPT BASE EXPONENT) if BASE has negative value and EXPONENT is quantity with uncertainty/error."))
  (let ((val (qexpt base (value exponent))))
    (make-quantity% :value val :error (abs (* (log base) val (aerr exponent))))))
(defmethod qexpt ((base quantity) (exponent integer))
  (qpow base exponent))
(defmethod qexpt ((base quantity) (exponent ratio))
  (let ((n (numerator exponent)) (d (denominator exponent)))
    (qroot (qpow base n) d)))
(defmethod qexpt ((base quantity) (exponent float))
  (unless (unitlessp base)
    (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT has a floating point value."))
  (let ((val (qexpt (value base) exponent)))
    (make-quantity% :value val :error (if (zerop (value base)) 0 (abs (/ (* exponent val (aerr base)) (value base)))))))
(defmethod qexpt ((base quantity) (exponent quantity))
  (unless (unitlessp exponent)
    (f-error invalid-unit-operation-error () "EXPONENT in the operation (QEXPT BASE EXPONENT) must be unitless."))
  (if (errorlessp exponent)
      (qexpt base (value exponent))
      ;; -> Exponent is unitless, but has an error. Base is anything.
      (progn
        (unless (unitlessp base)
          (f-error invalid-unit-operation-error () "BASE in operation (QEXPT BASE EXPONENT) must be unitless if EXPONENT is quantity with uncertainty/error."))
        ;; -> Exponent is unitless, but has an error. Base is unitless.
        (when (and (minusp (value base)) (has-error-p exponent))
          (f-error error-propagation-error () "Error propagation undefined for operation (QEXPT BASE EXPONENT) ~
                                               if BASE has negative value and EXPONENT is quantity with uncertainty/error."))
        (let* ((b (value base)) (db (aerr base)) (e (value exponent)) (de (aerr exponent)) (val (qexpt b e)))
          (make-quantity% :value val :error (if (zerop b) 0 (* (abs val) (sqrt (+ (expt (/ (* e db) b) 2) (expt (* (log b) de) 2))))))))))

(defgeneric qln (number))
(defmethod qln ((number real))
  (log number))
(defmethod qln ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QLN NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (log val) :error (abs (/ err val)))))
(export 'qln)

(defgeneric qlog (number base))
(defmethod qlog ((number real) (base real))
  (log number base))
(defmethod qlog ((number quantity) (base real))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QLOG NUMBER BASE) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (log val base) :error (abs (/ err val (log base))))))
(defmethod qlog ((number real) (base quantity))
  (unless (unitlessp base)
    (f-error invalid-unit-operation-error () "BASE in operation (QLOG NUMBER BASE) must be unitless."))
  (with-quantity-slots (base b db)
    (make-quantity% :value (log number b) :error (abs (/ (* (log number) db) b (expt (log b) 2))))))
(defmethod qlog ((number quantity) (base quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QLOG NUMBER BASE) must be unitless."))
  (unless (unitlessp base)
    (f-error invalid-unit-operation-error () "BASE in operation (QLOG NUMBER BASE) must be unitless."))
  (with-quantity-slots (number n dn)
    (with-quantity-slots (base b db)
      (make-quantity% :value (log n b) :error (sqrt (+ (expt (/ dn n (log b)) 2) (expt (/ (* (log n) db) b (expt (log b) 2)) 2)))))))
(export 'qlog)

(defgeneric qsin (number))
(defmethod qsin ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QSIN NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (sin val) :error (abs (* (cos val) err)))))
(defmethod qsin ((number real))
  (sin number))
(export 'qsin)

(defgeneric qcos (number))
(defmethod qcos ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QCOS NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (cos val) :error (abs (* (sin val) err)))))
(defmethod qcos ((number real))
  (cos number))
(export 'qcos)

(defgeneric qtan (number))
(defmethod qtan ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QTAN NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (tan val) :error (abs (/ err (expt (cos val) 2))))))
(defmethod qtan ((number real))
  (tan number))
(export 'qtan)

(defgeneric qasin (number))
(defmethod qasin ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QASIN NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (asin val) :error (abs (/ err (sqrt (- 1 (expt val 2))))))))
(defmethod qasin ((number real))
  (asin number))
(export 'qasin)

(defgeneric qacos (number))
(defmethod qacos ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QACOS NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (acos val) :error (abs (/ err (sqrt (- 1 (expt val 2))))))))
(defmethod qacos ((number real))
  (acos number))
(export 'qacos)

(defgeneric qatan (number))
(defmethod qatan ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QATAN NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (atan val) :error (abs (/ err (+ 1 (expt val 2)))))))
(defmethod qatan ((number real))
  (atan number))
(export 'qatan)

(defgeneric qsinh (number))
(defmethod qsinh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QSINH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (sinh val) :error (abs (* (cosh val) err)))))
(defmethod qsinh ((number real))
  (sinh number))
(export 'qsinh)

(defgeneric qcosh (number))
(defmethod qcosh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QCOSH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (cosh val) :error (abs (* (sinh val) err)))))
(defmethod qcosh ((number real))
  (cosh number))
(export 'qcosh)

(defgeneric qtanh (number))
(defmethod qtanh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QTANH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (tanh val) :error (abs (* (- 1 (expt (tanh val) 2)) err)))))
(defmethod qtanh ((number real))
  (tanh number))
(export 'qtanh)

(defgeneric qasinh (number))
(defmethod qasinh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QASINH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (asinh val) :error (abs (/ err (sqrt (+ (expt val 2) 1)))))))
(defmethod qasinh ((number real))
  (asinh number))
(export 'qasinh)

(defgeneric qacosh (number))
(defmethod qacosh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QACOSH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (acosh val) :error (abs (/ err (sqrt (- (expt val 2) 1)))))))
(defmethod qacosh ((number real))
  (acosh number))
(export 'qacosh)

(defgeneric qatanh (number))
(defmethod qatanh ((number quantity))
  (unless (unitlessp number)
    (f-error invalid-unit-operation-error () "NUMBER in operation (QATANH NUMBER) must be unitless."))
  (with-quantity-slots (number val err)
    (make-quantity% :value (atanh val) :error (abs (/ err (- 1 (expt val 2)))))))
(defmethod qatanh ((number real))
  (atanh number))
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
