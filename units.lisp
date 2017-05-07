(in-package :pq)

;; Unit interface ------------------------------------------------------------------

(defmacro mkunit (&rest expr)
  "Human interface to make a unit."
  `(dereference-unit (make-unit ,@(parseq 'unit expr))))
(export 'mkunit)

(defun make-unit (&rest unit-factors)
  "Machine interface for making a unit."
  (loop for uf in unit-factors collect
       (cond
         ((unit-factor-p uf) uf)
         ((and (listp uf) (l= uf 2) (stringp (first uf)) (integerp (second uf))) (make-uf (lookup-unit (first uf)) (second uf)))
         ((and (listp uf) (l= uf 2) (symbolp (first uf)) (integerp (second uf))) (make-uf (lookup-unit (symbol-name (first uf))) (second uf)))
         (t (f-error quantity-definition-syntax-error () "UNIT-FACTORS in (MAKE-UNIT &rest UNIT-FACTORS) must be lists of two elements: (LIST <SYMBOL> <INTEGER>).")))))
(export 'make-unit)

(defun unitp (object)
  (and (listp object) (every #'unit-factor-p object)))
(export 'unitp)

(defun has-unit-p (quantity)
  "Checks whether the given quantity has a unit."
  (consp (expand-unit (unit quantity))))

(defun unitlessp (quantity)
  "Checks whether the given quantity is unitless."
  (not (has-unit-p quantity)))

;; Unit expansion ------------------------------------------------------------------

(defun collect-factors (f &rest expanded-unit-factors)
  ;; Each of the expanded-unit-factors is a list (conv () () ())
  (destructuring-bind (conv units)
      (loop for factor in expanded-unit-factors
         collect (first factor) into conv-factors
         append (rest factor) into unit-factors
         finally (return (list conv-factors unit-factors)))
    `(,(apply #'* f conv) ,@units)))

(defun reduce-unit (unit)
  ;; Reduces the powers of duplicate unit factors in a given unit, e.g. km^2 / km -> km, but m / km -> m / km. No unit lookup is made.
  (if (l> unit 1)
      ;; Split the list of unit factors into those that have the same unit as the first one and those that do not
      (let* ((name (uf-unit (first unit)))
             (split (split #'(lambda (x) (equal x name)) unit :key #'uf-unit)))
        ;; Sum the powers of the first set
        (let ((sum (loop for item in (first split) summing (uf-power item))))
          ;; Append the resulting unit-factor
          (append (if (/= 0 sum) (list (make-uf name sum))) (reduce-unit (second split)))))
      ;; Only one unit factor. Keep everything as is.
      unit))

(defun expand-unit-factor (factor)
  ;; Converts a single unit factor into its expansion of base units, together with a conversion factor
  ;; Query the unit translation table
  (with-unit-lookup (base expansion (uf-unit factor))
    ;; When expansion is nil the unit is a base unit
    (if expansion
        ;; Not a base unit
        (destructuring-bind (conv unit-factors) expansion
          ;; Expand the unit collecting all conversion factors
          (apply #'collect-factors (expt conv (uf-power factor))
                 (loop for uf in unit-factors collect (expand-unit-factor (uf-pow uf (uf-power factor))))))
        ;; Base unit, no recursion
        (list 1 (make-uf base (uf-power factor))))))

(defun expand-unit (unit)
  ;; Expands the given unit into base units and reduces them
  (destructuring-bind (conv &rest unit-factors)
      (apply #'collect-factors 1 (loop for factor in unit collect (expand-unit-factor factor)))
    (values (reduce-unit unit-factors) conv)))

(defun dereference-unit (unit)
  ;; Takes a unit and looks up aliases and abbreviations of unit factors and replaces them with the base unit.
  (loop for uf in unit collect (make-uf (lookup-unit (uf-unit uf)) (uf-power uf))))

(defun units-equal (unit-a unit-b)
  ;; Reduces both units and compares the unit factors for equality (in unit and power)
  (let ((a (reduce-unit unit-a)) (b (reduce-unit unit-b)))
    (when (ll= a b)
      (loop for uf-a in a always
           (have uf-a b :test #'uf-equal)))))
(export 'units-equal)

(defun units-equalp (unit-a unit-b)
  ;; Reduces both units and compares the unit factors for equality (in unit and power)
  (multiple-value-bind (base-unit-a conv-a) (expand-unit unit-a)
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-b)
      (and (equalp conv-a conv-b) (units-equal base-unit-a base-unit-b)))))
(export 'units-equalp)

(defun units-convertible (unit-a unit-b)
  ;; Expands and reduces both units and compares the unit factors for equality (in unit and power)
  (units-equal (expand-unit unit-a) (expand-unit unit-b)))
(export 'units-convertible)

(defgeneric convert-unit% (value unit-a &optional unit-b))
(defmethod convert-unit% ((value number) unit-a &optional unit-b)
  (multiple-value-bind (base-unit-a conv-a) (expand-unit unit-a)
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-b)
      (unless (units-equal base-unit-a base-unit-b)
        (f-error invalid-unit-conversion-error () "Conversion between units ~a and ~a not possible (base units: ~a and ~a)." (str-unit unit-a) (str-unit unit-b) base-unit-a base-unit-b))
      (/ (* value conv-a) conv-b))))
(defmethod convert-unit% ((q quantity) unit-a &optional unit-b)
  (when unit-b
    (error "Overdefined unit conversion!"))
  (multiple-value-bind (base-unit-a conv-a) (expand-unit (unit q))
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-a)
      (unless (units-equal base-unit-a base-unit-b)
        (f-error invalid-unit-conversion-error () "Conversion between units ~a and ~a not possible (base units: ~a and ~a)." (str-unit q) (str-unit unit-a) base-unit-a base-unit-b))
      (make-quantity% :value (/ (* (value q) conv-a) conv-b) :error (if (minusp (error-direct q)) (error-direct q) (/ (* (error-direct q) conv-a) conv-b)) :unit unit-a))))

(defun convert-unit (quantity unit)
  (unless (quantityp quantity)
    (f-error operation-undefined-error () "QUANTITY in operation (CONVERT-UNIT QUANTITY UNIT) must be of type quantity."))
  (convert-unit% quantity (if (unitp unit) unit (apply #'make-unit unit))))
(export 'convert-unit)

(defun power-unit (unit power)
  (loop for uf in unit collect (uf-pow uf power)))

(defun multiply-units (&rest units)
  (reduce-unit (apply #'append units)))

(defun divide-units (&rest units)
  (if (l> units 1)
      (reduce-unit (append (first units) (apply #'append (mapcar #'(lambda (x) (power-unit x -1)) (rest units)))))
      (power-unit (first units) -1)))

(defun root-unit (unit index)
  (loop for uf in unit
     when (zerop (rem (uf-power uf) index))
     collect (make-uf (uf-unit uf) (/ (uf-power uf) index))
     else
     do (f-error operation-undefined-error () "INDEX in operation (ROOT-UNIT UNIT INDEX) is undefined if INDEX is ~a and UNIT is ~a." index (str-unit unit))))

(defun sort-unit (unit)
  (stable-sort unit #'(lambda (a b) (and (not (minusp a)) (minusp b))) :key #'uf-power))

(defun str-unit (obj)
  ;; Prints the given unit in human readable form
  (cond
    ;; nil
    ((null obj) "1")
    ;; number
    ((numberp obj) "1")
    ;; quantity
    ((quantityp obj) (str-unit (unit obj)))
    ;; unit
    ((listp obj) (with-output-to-string (stream)
                    ;; Loop over the unit factors
                    (loop
                       for uf in (sort-unit (copy-tree obj))
                       for i upfrom 0
                       ;; Insert a space unless it's before the first unit factor
                       when (plusp i) do (format stream " ")
                       do
                         (cond
                           ((and (minusp (uf-power uf)) (= (uf-power uf) -1)) (format stream "/ ~a" (uf-unit uf)))
                           ((and (minusp (uf-power uf)) (< (uf-power uf) -1)) (format stream "/ ~a ^ ~a" (uf-unit uf) (- (uf-power uf))))
                           ((= (uf-power uf) 1) (format stream "~a" (uf-unit uf)))
                           (t (format stream "~a ^ ~a" (uf-unit uf) (uf-power uf)))))))
    (t (f-error operation-undefined-error () "Operation (STR-UNIT OBJ) is undefined if OBJ is of type ~a." (type-of obj)))))
(export 'str-unit)
