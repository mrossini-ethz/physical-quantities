(in-package :pq)

;; Unit prefix database ------------------------------------------------------------

(defparameter *unit-prefix-table* (make-hash-table :test 'equal))
(export '*unit-prefix-table*)

(defmacro define-unit-prefixes (&body prefix-declarations)
  `(progn
     ,@(loop for decl in prefix-declarations collect
            (destructuring-bind (name abbr power &key (base 10)) decl
              `(setf (gethash ,(symbol-name name) *unit-prefix-table*) (list ,base ,power ,(symbol-name abbr)))))))
(export 'define-unit-prefixes)

;; Unit database -------------------------------------------------------------------

(defparameter *unit-translation-table* (make-hash-table :test 'equal))
(defparameter *unit-alias-table* (make-hash-table :test 'equal))
(defparameter *unit-abbreviation-table* (make-hash-table :test 'equal))
(export '(*unit-translation-table* *unit-alias-table* *unit-abbreviation-table*))

(defun table-insert (name aliases abbrevs def)
  ;; Set main entry
  (setf (gethash (symbol-name name) *unit-translation-table*) def)
  (loop for alias in (mklist aliases) do
       (setf (gethash (symbol-name alias) *unit-alias-table*) (symbol-name name)))
  (loop for abbrev in (mklist abbrevs) do
       (setf (gethash (symbol-name abbrev) *unit-abbreviation-table*) (symbol-name name))))

(defun symbol-prefix (prefix symbols)
  (mapcar #'(lambda (x) (symb prefix x)) (mklist symbols)))

(defmacro define-units (&body unit-declarations)
  (let (names aliases abbrevs)
    (with-gensyms (prefix v)
      `(progn
         ;; Loop over all statements
         ,@(loop for decl in unit-declarations append
                ;; Destructure each declaration
                (destructuring-bind (name &key def alias abbrev prefix-max prefix-min (base 10)) decl
                  `(,@(progn
                       ;; Bookkeeping of newly defined names, aliases and abbreviations (does not generate code)
                       (if (have name names) (warn "Unit ~a is already defined." name) (push name names))
                       (loop for a in (mklist alias) when (have a aliases) do (warn "Unit alias ~a is already defined." a) else do (push a aliases))
                       (loop for a in (mklist abbrev) when (have a abbrevs) do (warn "Unit abbreviation ~a is already defined." a) else do (push a abbrevs)))
                      ;; Add the names, aliases and abbreviations to the respective tables
                      (table-insert ',name ',alias ',abbrev (list ,@(parseq 'unit-definition def)))
                      (loop
                         for ,prefix being the hash-keys of *unit-prefix-table* using (hash-value ,v)
                         when (and ,(or (not prefix-max) `(<= (second ,v) ,prefix-max))
                                   ,(or (not prefix-min) `(>= (second ,v) ,prefix-min))
                                   ,(if (listp base) `(have (first ,v) (list ,@base)) `(= ,base (first ,v)))) do
                         ;; Add main name to *unit-translation-table*
                           (table-insert (symb ,prefix ',name)
                                         (symbol-prefix ,prefix ',alias)
                                         (symbol-prefix (third ,v) ',abbrev)
                                         (if (zerop (second ,v)) ',def (list (expt (first ,v) (second ,v)) (list (make-uf ,(symbol-name name) 1)))))))))))))
(export 'define-units)

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

;; Unit interface ------------------------------------------------------------------

(defun make-unit (&rest unit-factors)
  (loop for uf in unit-factors collect
       (cond
         ((unit-factor-p uf) uf)
         ((and (listp uf) (l= uf 2) (stringp (first uf)) (integerp (second uf))) (make-uf (lookup-unit (first uf)) (second uf)))
         ((and (listp uf) (l= uf 2) (symbolp (first uf)) (integerp (second uf))) (make-uf (lookup-unit (symbol-name (first uf))) (second uf)))
         (t (error "Invalid unit factor specified.")))))
(export 'make-unit)

(defun unitp (object)
  (and (listp object) (every #'unit-factor-p object)))
(export 'unitp)

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

(defgeneric convert-unit% (value unit-a &optional unit-b))
(defmethod convert-unit% ((value number) unit-a &optional unit-b)
  (multiple-value-bind (base-unit-a conv-a) (expand-unit unit-a)
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-b)
      (unless (units-equal base-unit-a base-unit-b)
        (error "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" (print-unit unit-a) (print-unit unit-b) base-unit-a base-unit-b))
      (/ (* value conv-a) conv-b))))
(defmethod convert-unit% ((q quantity) unit-a &optional unit-b)
  (when unit-b
    (error (format nil "Overdefined unit conversion!")))
  (multiple-value-bind (base-unit-a conv-a) (expand-unit (unit q))
    (multiple-value-bind (base-unit-b conv-b) (expand-unit unit-a)
      (unless (units-equal base-unit-a base-unit-b)
        (error "Cannot convert unit ~a into ~a (base units: ~a -> ~a)!" (print-unit (unit q)) (print-unit unit-a) base-unit-a base-unit-b))
      (make-quantity% :value (/ (* (value q) conv-a) conv-b) :error (if (minusp (error-direct q)) (error-direct q) (/ (* (error-direct q) conv-a) conv-b)) :unit unit-a))))

(defun convert-unit (quantity unit)
  (unless (quantityp quantity)
    (error "Quantity must be of type quantity."))
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
     do (error (format nil "Cannot extract the ~:r root of the unit ~a!" index unit))))

(defun sort-unit (unit)
  (stable-sort unit #'(lambda (a b) (and (not (minusp a)) (minusp b))) :key #'uf-power))

(defun print-unit (unit)
  ;; Prints the given unit in human readable form
  (if unit
      ;; List of unit factors is not empty
      (with-output-to-string (stream)
        ;; Loop over the unit factors
        (loop
           for uf in (sort-unit (copy-tree unit))
           for i upfrom 0
           ;; Insert a space unless it's before the first unit factor
           when (plusp i) do (format stream " ")
           do
             (cond
               ((and (minusp (uf-power uf)) (= (uf-power uf) -1)) (format stream "/ ~a" (uf-unit uf)))
               ((and (minusp (uf-power uf)) (< (uf-power uf) -1)) (format stream "/ ~a ^ ~a" (uf-unit uf) (- (uf-power uf))))
               ((= (uf-power uf) 1) (format stream "~a" (uf-unit uf)))
               (t (format stream "~a ^ ~a" (uf-unit uf) (uf-power uf))))))
      ;; List of unit factors is empty, therefore unit 1
      "1"))
