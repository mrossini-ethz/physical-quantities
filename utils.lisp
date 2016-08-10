(in-package :pq)

(defun xnor (&rest forms)
  (evenp (count-if #'identity forms)))

(defmacro nand (&rest forms)
  `(not (and ,@forms)))

(defun mklist (obj)
  "Makes a list out of the given object unless it already is a list."
  (if (listp obj) obj (list obj)))

(defun ll= (list-a list-b)
  "Tests efficiently whether the length of list-a is equal to the length of list-b."
  (if (nand list-a list-b)
      (xnor list-a list-b)
      (ll= (rest list-a) (rest list-b))))

(defun l= (list length)
  "Tests efficiently whether the length of the list is equal to the given length."
  (cond
    ((null list) (= 0 length))
    ((zerop length) (null list))
    (t (l= (rest list) (- length 1)))))

(defun l> (list length)
  "Tests efficiently whether the length of the list is greater than the given length."
  (cond
    ((null list) nil)
    ((zerop length) (consp list))
    (t (l> (rest list) (- length 1)))))

(defun split (test sequence &key (key #'identity))
  (loop for item in sequence for result = (funcall test (funcall key item)) when result collect item into a else collect item into b finally (return (list a b))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym (concatenate 'string (symbol-name ',n) "-"))))
     ,@body))
