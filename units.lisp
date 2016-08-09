(in-package :pq)

(defparameter *unit-prefixes* (make-hash-table))
;; FIXME: make prefixes definable
(setf (gethash -24 *unit-prefixes*) '("yocto" "y"))
(setf (gethash -21 *unit-prefixes*) '("zepto" "z"))
(setf (gethash -18 *unit-prefixes*) '("atto" "a"))
(setf (gethash -15 *unit-prefixes*) '("femto" "f"))
(setf (gethash -12 *unit-prefixes*) '("nano" "n"))
(setf (gethash -9 *unit-prefixes*) '("pico" "p"))
(setf (gethash -6 *unit-prefixes*) '("micro" "u"))
(setf (gethash -3 *unit-prefixes*) '("milli" "m"))
(setf (gethash -2 *unit-prefixes*) '("centi" "c"))
(setf (gethash -1 *unit-prefixes*) '("deci" "d"))
(setf (gethash 0 *unit-prefixes*) '("" ""))
(setf (gethash 1 *unit-prefixes*) '("deca" "da"))
(setf (gethash 2 *unit-prefixes*) '("hecto" "h"))
(setf (gethash 3 *unit-prefixes*) '("kilo" "k"))
(setf (gethash 6 *unit-prefixes*) '("mega" "meg"))
(setf (gethash 9 *unit-prefixes*) '("giga" "g"))
(setf (gethash 12 *unit-prefixes*) '("tera" "t"))
(setf (gethash 15 *unit-prefixes*) '("peta" "pet"))
(setf (gethash 18 *unit-prefixes*) '("exa" "e"))
(setf (gethash 21 *unit-prefixes*) '("zetta" "zet"))
(setf (gethash 24 *unit-prefixes*) '("yotta" "yot"))

(defrule unit-definition () (and form (* unit-factor)) (:destructure (conv unit-factors) `(,conv (list ,@unit-factors))))

(defmacro define-units (&body unit-declarations)
  `(progn ,@(loop for decl in unit-declarations append
                 (destructuring-bind (unit &key def &allow-other-keys) decl
                   (loop for k being the hash-keys of *unit-prefixes* using (hash-value v) collect
                        `(setf (gethash ',(symb (string-upcase (first v)) unit) *unit-translation-table*)
                               ,(if (zerop k) (if def `(list ,@(parse-list 'unit-definition def))) `(list (expt 10 ,k) (list (list ',unit 1))))))))))

(defparameter *unit-translation-table* (make-hash-table))

(define-units
  ;; SI base units
  (metre :abbrev m :prefix-max 3)
  (gram :abbrev g :prefix-max 3)
  (second :abbrev s :prefix-max 0)
  (ampere :abbrev a)
  (kelvin :abbrev k :prefix-max 0)
  (mol)
  (candela :abbrev cd)
  ;; SI derived units
  (radian :def (1))
  (steradian :def (1))
  (hertz :def (1 / second))
  (newton :def (1 kilogram metre / second ^ 2))
  (pascal :def (1 newton / metre ^ 2))
  (joule :def (1 newton metre))
  (watt :def (1 joule / second))
  (coulomb :def (1 ampere second))
  (volt :def (1 watt / ampere))
  (farad :def (1 coulomb / volt))
  (ohm :def (1 volt / ampere))
  (siemens :def (1 ampere / volt))
  (weber :def (1 volt second))
  (tesla :def (1 weber / metre ^ 2))
  (henry :def (1 weber / ampere))
  (celsius :def (1 kelvin))
  (lumen :def (1 candela steradian))
  (lux :def (1 lumen / metre ^ 2))
  (becquerel :def (1 / second))
  (gray :def (1 joule / kilogram))
  (sievert :def (1 joule / kilogram))
  (katal :def (1 mol / second))
)

(defun collect-factors (f &rest expanded-unit-factors)
  ;; Each of the expanded-unit-factors is a list (conv () () ())
  (destructuring-bind (conv units)
      (loop for factor in expanded-unit-factors
         collect (first factor) into conv-factors
         append (rest factor) into unit-factors
         finally (return (list conv-factors unit-factors)))
    `(,(apply #'* f conv) ,@units)))

(defun reduce-unit (unit)
  (if (l> unit 1)
      (let ((split (split #'(lambda (x) (eql x (caar unit))) unit :key #'first)))
        (let ((sum (loop for item in (first split) summing (second item))))
          (append (if (/= 0 sum) (list (list (caaar split) sum))) (reduce-unit (second split)))))
      unit))

(defun expand-unit-factor (factor)
  ;; Input:
  ;; - `factor' is a unit factor, e.g. (kJ 2)
  ;; Output:
  ;; - a list of conversion factor and unit factors (1000 (kg 2) (m 4) (s -4))
  (destructuring-bind (unit power) factor
    ;; Query the unit translation table
    (multiple-value-bind (result found) (gethash unit *unit-translation-table*)
      (if found
          ;; When result is nil the unit is a base unit
          (if result
              ;; Not a base unit
              (destructuring-bind (conv unit-factors) result
                ;; Expand the unit collecting all conversion factors
                (apply #'collect-factors (expt conv power)
                       (loop for uf in unit-factors collect (expand-unit-factor `(,(first uf) ,(* (second uf) power))))))
              ;; Base unit, no recursion
              (list 1 factor))
          (error "Illegal unit specified!")))))

(defun expand-unit (unit)
  ;; `unit' is a list of unit factors, e.g. ((kN 1) (mm 1))
  (destructuring-bind (conv &rest unit-factors)
      (apply #'collect-factors 1 (loop for factor in unit collect (expand-unit-factor factor)))
    (values (reduce-unit unit-factors) conv)))

(defun units-equal (unit-a unit-b)
  (when (ll= unit-a unit-b)
    (loop for item-a in unit-a always
         (let ((item-b (find (first item-a) unit-b :key #'first)))
           (and item-b (= (second item-a) (second item-b)))))))

(defgeneric convert-units (value unit-a &optional unit-b))
(defmethod convert-units ((value number) unit-a &optional unit-b)
  (multiple-value-bind (base-unit-a conv-a) (expand-unit unit-a)
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-b)
      (unless (units-equal base-unit-a base-unit-b)
        (error (format nil "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" unit-a unit-b base-unit-a base-unit-b)))
      (/ (* value conv-a) conv-b))))
(defmethod convert-units ((q quantity) unit-a &optional unit-b)
  (when unit-b
    (error (format nil "Overdefined unit conversion!")))
  (multiple-value-bind (base-unit-a conv-a) (expand-unit (unit q))
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-a)
      (unless (units-equal base-unit-a base-unit-b)
        (error (format nil "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" unit-a unit-b base-unit-a base-unit-b)))
      (make-instance 'quantity :value (/ (* (value q) conv-a) conv-b) :error (if (minusp (error-direct q)) (error-direct q) (/ (* (error-direct q) conv-a) conv-b)) :unit unit-a))))

(defun multiply-units (&rest units)
  (reduce-unit (apply #'append units)))

(defun divide-units (&rest units)
  (assert (l> units 1))
  (reduce-unit (append (first units) (loop for unit in (rest units) append (loop for unit-factor in unit collect `(,(first unit-factor) ,(- (second unit-factor))))))))

