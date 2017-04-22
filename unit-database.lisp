(in-package :pq)

;; Unit prefix database ------------------------------------------------------------

(defparameter *unit-prefix-table* (make-hash-table :test 'equal))

(defun define-unit-prefix% (name power &key abbrev (base 10))
  "Defines a unit prefix such as kilo in kilometre. Apart from the name the power is required (3 for kilo) together with the base that defaults to 10. An abbreviation for the prefix is also allowed which will be used in combination with abbreviated units."
  ;; Check inputs
  (unless (symbolp name)
    (error "Unit prefix must be a symbol."))
  (unless (symbolp abbrev)
    (error "Unit prefix abbreviation must be a symbol."))
  (unless (and (integerp base) (plusp base))
    (error "Unit prefix base must be an integer greater than zero."))
  (unless (and (integerp power) (not (zerop power)))
    (error "Unit prefix power must be an integer different from zero."))
  (let ((name-str (symbol-name name)) (abbrev-str (if abbrev (symbol-name abbrev))))
    ;; Check for name conflicts. Names and abbreviations can be treated separately.
    (when (has-key name-str *unit-prefix-table*)
      (error "Unit prefix ~a is already defined." name-str))
    (when abbrev-str
      (loop for v being the hash-values of *unit-prefix-table* when (string= abbrev-str (third v)) do
           (error "Unit prefix ~a is already defined." abbrev-str)))
    ;; Add the prefixes to the hash table
    (setf (gethash name-str *unit-prefix-table*) (list base power abbrev-str))
    ;; FIXME: add prefixes to existing units
    ))

(defmacro define-unit-prefix (name power &key abbrev (base 10))
  `(define-unit-prefix% ',name ,power :abbrev ',abbrev :base ,base))
(export 'define-unit-prefix)

;; Unit prefix composition functions -----------------------------------------------

(defun prefix-and (&rest functions)
  (lambda (base power)
    (loop for f in functions always (funcall f base power))))

(defun prefix-or (&rest functions)
  (lambda (base power)
    (loop for f in functions thereis (funcall f base power))))

(defun prefix-range (base power-min power-max)
  (lambda (b p)
    (and (= b base) (or (not power-min) (>= p power-min)) (or (not power-max) (<= p power-max)))))

(defun prefix-list (base &rest powers)
  (lambda (b p)
    (and (= b base) (have p powers))))

(defun prefix-base (base &optional mod)
  (lambda (b p)
    (and (= b base) (or (not mod) (zerop (rem p mod))))))

;; FIXME: create a macro that defines a language for defining prefixes validation functions

;; Unit database -------------------------------------------------------------------

(defparameter *unit-translation-table* (make-hash-table :test 'equal))
(defparameter *unit-alias-table* (make-hash-table :test 'equal))
(defparameter *unit-abbreviation-table* (make-hash-table :test 'equal))

(defun clear-units ()
  (setf *unit-prefix-table* (make-hash-table :test 'equal))
  (setf *unit-translation-table* (make-hash-table :test 'equal))
  (setf *unit-alias-table* (make-hash-table :test 'equal))
  (setf *unit-abbreviation-table* (make-hash-table :test 'equal)))

(defun table-insert (name aliases abbrevs def)
  ;; Set main entry
  (setf (gethash (symbol-name name) *unit-translation-table*) def)
  ;; Set aliases
  (loop for alias in (mklist aliases) do
       (setf (gethash (symbol-name alias) *unit-alias-table*) (symbol-name name)))
  ;; Set abbreviations
  (loop for abbrev in (mklist abbrevs) do
       (setf (gethash (symbol-name abbrev) *unit-abbreviation-table*) (symbol-name name))))

(defun unit-hash-key-check (key)
  (or (has-key (symbol-name key) *unit-translation-table*)
      (has-key (symbol-name key) *unit-alias-table*)
      (has-key (symbol-name key) *unit-abbreviation-table*)))

(defun table-check (name aliases abbrevs)
  (when (unit-hash-key-check name)
    (error "Unit ~a is already defined." name))
  (loop for alias in (mklist aliases)
     when (unit-hash-key-check alias)
     do (error "Unit ~a is already defined." alias))
  (loop for abbrev in (mklist abbrevs)
     when (unit-hash-key-check abbrev)
     do (error "Unit ~a is already defined." abbrev)))

(defun symbol-prefix (prefix symbols)
  (mapcar #'(lambda (x) (symb prefix x)) (mklist symbols)))

(defun define-unit% (name &key def aliases abbreviations prefix-test overwrite)
  "Defines a new unit with the identifier name. A list of aliases and a list of abbreviations are permitted which - when encountered - are internally converted to the primary identifier. The definition allows the unit to be defined in terms of other units, e.g :def (1.602 kilometre). Prefixes is must be a function of two parameters, the base and the power, with which it decides whether a prefix is allowed for the unit. It defaults to allowing all defined prefixes."
  ;; FIXME: the prefix test needs to be stored to allow new prefixes to be defined after defining the unit.
  ;; First pass to check for conflicts
  (unless overwrite
    ;; Check principal entry
    (table-check name aliases abbreviations)
    ;; Iterate over all prefixes
    (loop for prefix being the hash-keys of *unit-prefix-table* using (hash-value prefix-value)
       ;; Define the hash keys
       for name-key = (symb prefix name)
       for alias-keys = (symbol-prefix prefix aliases)
       for abbrev-keys = (symbol-prefix (third prefix-value) abbreviations)
       when (or (not prefix-test) (funcall prefix-test (first prefix-value) (second prefix-value))) do
         (table-check name-key alias-keys abbrev-keys)))
  ;; Second pass to insert the values
  (table-insert name aliases abbreviations def)
  (loop for prefix being the hash-keys of *unit-prefix-table* using (hash-value prefix-value)
     ;; Define the hash keys
     for name-key = (symb prefix name)
     for alias-keys = (symbol-prefix prefix aliases)
     for abbrev-keys = (symbol-prefix (third prefix-value) abbreviations)
     when (or (not prefix-test) (funcall prefix-test (first prefix-value) (second prefix-value))) do
       (table-insert name-key alias-keys abbrev-keys (if (zerop (second prefix-value)) def (list (expt (first prefix-value) (second prefix-value)) (list (make-uf (symbol-name name) 1)))))))

(defmacro define-unit (name &key def alias abbrev prefix-test overwrite)
  `(define-unit% ',name :def (list ,@(parseq 'unit-definition def)) :aliases ',alias :abbreviations ',abbrev :prefix-test ,prefix-test :overwrite ,overwrite))
(export 'define-unit)

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

;; Namespace macros -----------------------------------------------------------

(defmacro with-local-units (&body body)
  "Shadow the global unit table with a new rule table."
  `(let ((*unit-translation-table* (make-hash-table :test 'equal))
         (*unit-alias-table* (make-hash-table :test 'equal))
         (*unit-abbreviation-table* (make-hash-table :test 'equal))
         (*unit-prefix-table* (make-hash-table :test 'equal)))
     ;; Execute the body
     ,@body))
(export 'with-local-units)

(defmacro with-saved-units (&body body)
  "Shadow the global unit table with a copy of the unit table. When returninng from the body the original units are restored."
  `(let ((*unit-translation-table* (copy-hash-table *unit-translation-table*))
         (*unit-alias-table* (copy-hash-table *unit-alias-table*))
         (*unit-abbreviation-table* (copy-hash-table *unit-abbreviation-table*))
         (*unit-prefix-table* (copy-hash-table *unit-prefix-table*)))
     ;; Execute the body
     ,@body))
(export 'with-saved-units)
