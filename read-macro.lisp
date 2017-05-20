(in-package :pq)

(defun read-quantity (stream char1 char2)
  "The read macro #q(...) is an abbreviation for (quantity ...) which does not transform lowercase symbols to uppercase for unit factors."
  (declare (ignore char2))
  (let (items)
    ;; Read the opening bracket
    (when (char/= (read-char stream) #\()
      (error "Error reading quantity. #~c must be followed by a list." char1))
    ;; Perform minimal parsing of the stream to decide whether or not to perserve the letter case
    (when (char= (peek-char t stream t) #\))
      (error "Error reading quantity: #~c()" char1))
    ;; Read the value of the quantity (do not preserve case)
    (push (read stream t nil t) items)
    ;; Test for the closing bracket
    (unless (char= (peek-char t stream t) #\))
      ;; Read the next symbol (preserve case because it's either the symbol +/- or +- or a unit)
      (let ((*readtable* (copy-readtable *readtable*)))
        (setf (readtable-case *readtable*) :preserve)
        (push (read stream t nil t) items))
      ;; Check whether an uncertainty is specified
      (when (or (eql (first items) '+/-) (eql (first items) '+-))
        ;; Read the uncertainty (do not preserve case)
        (when (char= (peek-char t stream t) #\))
          (error "Error reading quantity #~c(~a ~a): ~a must be followed by an uncertainty/error." char1 (second items) (first items) (first items)))
        (push (read stream t nil t) items))
      ;; Read the rest of the list (preserving case)
      (let ((*readtable* (copy-readtable *readtable*)))
        (setf (readtable-case *readtable*) :preserve)
        (loop until (char= (peek-char t stream t) #\)) do
             (push (read stream t nil t) items))))
    ;; Read the closing bracket
    (read-char stream)
    ;; Expand the list into a macro call
    `(quantity ,@(nreverse items))))

(defun read-unit (stream char1 char2)
  "The read macro #u(...) is an abbreviation for (mkunit ...) which does not transform lowercase symbols to uppercase."
  (declare (ignore char1 char2))
  `(mkunit ,@(let ((*readtable* (copy-readtable *readtable*)))
               ;; Change the readtable such that case is preserved
               (setf (readtable-case *readtable*) :preserve)
               (read stream t nil t))))

(defun define-read-macros (&key (quantity #\q) (unit #\u))
  "Lets the user define the #q(...) and #u(...) read macros."
  (when quantity
    (set-dispatch-macro-character #\# quantity #'read-quantity))
  (when unit
    (set-dispatch-macro-character #\# unit #'read-unit)))
(export 'define-read-macros)
