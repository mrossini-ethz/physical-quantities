(in-package :pq)

;; Unit prefix database ------------------------------------------------------------

(defparameter *unit-prefix-table* (make-hash-table))
(export '*unit-prefix-table*)

(defmacro define-unit-prefixes (&body prefix-declarations)
  `(progn
     ,@(loop for decl in prefix-declarations collect
            (destructuring-bind (name abbr power &key (base 10)) decl
              `(setf (gethash ',name *unit-prefix-table*) (list ,base ,power ',abbr))))))

;; Unit database -------------------------------------------------------------------

(defparameter *unit-translation-table* (make-hash-table))
(defparameter *unit-alias-table* (make-hash-table))
(defparameter *unit-abbreviation-table* (make-hash-table))
(export '(*unit-translation-table* *unit-alias-table* *unit-abbreviation-table*))

(defmacro define-units (&body unit-declarations)
  (let (names aliases abbrevs)
  `(progn
     ;; Loop over all statements
     ,@(loop for decl in unit-declarations append
            ;; Destructure each declaration
            (destructuring-bind (name &key def alias abbrev prefix-max prefix-min (base 10)) decl
              ;; Add the names, aliases and abbreviations to the respective tables
              (append
               ;; Bookkeeping of newly defined names, aliases and abbreviations
               (progn
                 (if (have name names) (warn "Unit ~a is already defined." name) (push name names))
                 (loop for a in (mklist alias) when (have a aliases) do (warn "Unit alias ~a is already defined." a) else do (push a aliases))
                 (loop for a in (mklist abbrev) when (have a abbrevs) do (warn "Unit abbreviation ~a is already defined." a) else do (push a abbrevs)))
               `((setf (gethash ',name *unit-translation-table*) ,(if def `(list ,@(parse-list 'unit-definition def))))
                 ,@(loop for a in (mklist alias) collect
                        `(setf (gethash ',a *unit-alias-table*) ',name))
                 ,@(loop for a in (mklist abbrev) collect
                        `(setf (gethash ',a *unit-abbreviation-table*) ',name)))
               ;; Add the names, aliases and abbreviations with all possible prefixes to the respective tables
               (loop
                  for prefix being the hash-keys of *unit-prefix-table* using (hash-value v)
                  for pbase = (first v)
                  for ppower = (second v)
                  for prefix-abbrev = (third v)
                  when (and (or (not prefix-max) (<= ppower prefix-max)) (or (not prefix-min) (>= ppower prefix-min)) (have pbase (mklist base))) append
                    (append
                     ;; Add main name to *unit-translation-table*
                     (list `(setf (gethash ',(symb prefix name) *unit-translation-table*)
                                  ,(if (zerop ppower) (if def `(list ,@(parse-list 'unit-definition def))) `(list (expt ,pbase ,ppower) (list (list ',name 1))))))
                     ;; Add aliases to *unit-alias-table*, with all possible prefixes, that point to the names in *unit-translation-table*
                     (loop for a in (mklist alias) collect
                          `(setf (gethash ',(symb prefix a) *unit-alias-table*) ',(symb prefix name)))
                     ;; Add abbreviations to *unit-abbreviation-table*, with all possible prefixes, that point to the names in *unit-translation-table*
                     (loop for a in (mklist abbrev) collect
                          `(setf (gethash ',(symb prefix-abbrev a) *unit-alias-table*) ',(symb prefix name)))))))))))

(defun lookup-unit (unit)
  ;; Search the translation table directly
  (multiple-value-bind (result found) (gethash unit *unit-translation-table*)
    (if found
        (values unit result)
        ;; Not found, search the alias table instead
        (multiple-value-bind (result found) (gethash unit *unit-alias-table*)
          (if found
              (lookup-unit result)
              ;; Still not found, search the abbreviation table instead
              (multiple-value-bind (result found) (gethash unit *unit-abbreviation-table*)
                (if found
                    (lookup-unit result)
                    (error "Unknown unit ~a!" unit))))))))

(defmacro with-unit-lookup ((base-unit translation unit) &body body)
  `(multiple-value-bind (,base-unit ,translation) (lookup-unit ,unit)
     ,@body))

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
    (with-unit-lookup (base result unit)
      ;; When result is nil the unit is a base unit
      (if result
          ;; Not a base unit
          (destructuring-bind (conv unit-factors) result
            ;; Expand the unit collecting all conversion factors
            (apply #'collect-factors (expt conv power)
                   (loop for uf in unit-factors collect (expand-unit-factor `(,(first uf) ,(* (second uf) power))))))
          ;; Base unit, no recursion
          (list 1 `(,base ,power))))))

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
        (error "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" (print-unit unit-a) (print-unit unit-b) base-unit-a base-unit-b))
      (/ (* value conv-a) conv-b))))
(defmethod convert-units ((q quantity) unit-a &optional unit-b)
  (when unit-b
    (error (format nil "Overdefined unit conversion!")))
  (multiple-value-bind (base-unit-a conv-a) (expand-unit (unit q))
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-a)
      (unless (units-equal base-unit-a base-unit-b)
        (error "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" (print-unit (unit q)) (print-unit unit-a) base-unit-a base-unit-b))
      (make-instance 'quantity :value (/ (* (value q) conv-a) conv-b) :error (if (minusp (error-direct q)) (error-direct q) (/ (* (error-direct q) conv-a) conv-b)) :unit unit-a))))

(defun power-unit (unit power)
  (loop for uf in unit collect `(,(first uf) ,(* (second uf) power))))

(defun multiply-units (&rest units)
  (reduce-unit (apply #'append units)))

(defun divide-units (&rest units)
  (if (l> units 1)
      (reduce-unit (append (first units) (apply #'append (mapcar #'(lambda (x) (power-unit x -1)) (rest units)))))
      (power-unit (first units) -1)))

(defun root-unit (unit index)
  (loop for uf in unit
     when (zerop (rem (second uf) index))
     collect `(,(first uf) ,(/ (second uf) index))
     else
     do (error (format nil "Cannot extract the ~:r root of the unit ~a!" index unit))))

(defun sort-unit (unit)
  (stable-sort unit #'(lambda (a b) (and (not (minusp a)) (minusp b))) :key #'second))

(defun print-unit (unit)
  (with-output-to-string (stream)
    (loop
       for uf in (sort-unit (copy-tree unit))
       for i upfrom 0
       when (plusp i) do (format stream " ")
       do
         (cond
            ((and (minusp (second uf)) (= (second uf) -1)) (format stream "/ ~a" (first uf)))
            ((and (minusp (second uf)) (< (second uf) -1)) (format stream "/ ~a ^ ~a" (first uf) (- (second uf))))
            ((= (second uf) 1) (format stream "~a" (first uf)))
            (t (format stream "~a ^ ~a" (first uf) (second uf)))))))
