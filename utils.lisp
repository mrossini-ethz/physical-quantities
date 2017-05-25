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

(defun unzip (list)
  (let ((n (list-length list)))
    (unless (evenp n)
      (error "Number of items in list must be even."))
    (loop for i below (/ n 2) collect (nth (* 2 i) list) into a collect (nth (1+ (* 2 i)) list) into b finally (return (list a b)))))

(defun have (item sequence &key (test #'eql) (key #'identity))
  ;; Checks whether the given item is in the list
  (some #'(lambda (x) (funcall test item (funcall key x))) sequence))

(defun has-key (key hash-table)
  (second (multiple-value-list (gethash key hash-table))))

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

(defun copy-hash-table (hash-table)
  "Creates a copy of the given hash table."
  (loop for k being the hash-keys in hash-table using (hash-value v)
     with ht = (make-hash-table :test (hash-table-test hash-table))
     do (setf (gethash k ht) v)
     finally (return ht)))

;; Math Utilities =====================================================

(defun py+ (&rest numbers)
  "Pythagorean addition"
  (sqrt (coerce (apply #'+ (mapcar (lambda (x) (* x x)) numbers)) 'double-float)))

;; Error function -----------------------------------------------------

(defun erf (x)
  "Calculates the error function."
  ;; Algorithm: Numerical Recipes in Fortran 77: The Art of Scientific Computing
  (let ((tee (/ 1 (+ 1 (/ (abs x) 2)))) tau)
    (setf tau (* tee (exp (+ (- (expt x 2))
                             -1.26551223
                             (* +1.00002368 tee)
                             (* +0.37409196 (expt tee 2))
                             (* +0.09678418 (expt tee 3))
                             (* -0.18628806 (expt tee 4))
                             (* +0.27886807 (expt tee 5))
                             (* -1.13520398 (expt tee 6))
                             (* +1.48851587 (expt tee 7))
                             (* -0.82215223 (expt tee 8))
                             (* +0.17087277 (expt tee 9))))))
    (if (minusp x)
        (- tau 1)
        (- 1 tau))))

(defun p-value (x)
  (/ (1+ (erf (/ x #.(sqrt 2)))) 2))

;; Inverse error function ---------------------------------------------

(defun chebyshev-polynomial% (n x a b)
  "Recursion function for the Chebyshev polynomial evaluation."
  ;; Derived from Fibonacci series algorithm: Structure and Interpretation of Computer Programs
  (if (= n 0) b (chebyshev-polynomial% (1- n) x (- (* 2 x a) b) a)))

(defun chebyshev-polynomial (n x)
  "Evaluates the nth Chebyshev polynomial."
  (chebyshev-polynomial% n x x 1))

(defun beta (x)
  "Helper for the inverf function."
  (sqrt (- (log (- 1 (expt x 2))))))

(defun r-function-a (x)
  "Helper for the inverf function."
  (let ((y (+ (* -1.548813042373261659512742l0 (beta x)) 2.565490123147816151928163l0))
        (l #(+0.9121588034175537733059200l0
             -0.0162662818676636958546661l0
             +0.0004335564729494453650589l0
             +0.0002144385700744592065205l0
             +0.0000026257510757648130176l0
             -0.0000030210910501037969912l0
             -0.0000000124060618367572157l0
             +0.0000000624066092999917380l0
             -0.0000000005401247900957858l0
             -0.0000000014232078975315910l0
             +0.0000000000343840281955305l0
             +0.0000000000335848703900138l0
             -0.0000000000014584288516512l0
             -0.0000000000008102174258833l0
             +0.0000000000000525324085874l0
             +0.0000000000000197115408612l0
             -0.0000000000000017494333828l0
             -0.0000000000000004800596619l0
             +0.0000000000000000557302987l0
             +0.0000000000000000116326054l0
             -0.0000000000000000017262489l0
             -0.0000000000000000002784973l0
             +0.0000000000000000000524481l0
             +0.0000000000000000000065270l0
             -0.0000000000000000000015707l0
             -0.0000000000000000000001475l0
             +0.0000000000000000000000450l0)))
    (loop for n upto 26 sum (* (elt l n) (chebyshev-polynomial n y)))))

(defun r-function-b (x)
  "Helper for the inverf function."
  (let ((y (+ (* -0.5594576313298323225436913l0 (beta x)) 2.287915716263357638965891l0))
        (l #(+0.9566797090204925274526373l0
             -0.0231070043090649036999908l0
             -0.0043742360975084077333218l0
             -0.0005765034226511854809364l0
             -0.0000109610223070923931242l0
             +0.0000251085470246442787982l0
             +0.0000105623360679477511955l0
             +0.0000027544123300306391503l0
             +0.0000004324844983283380689l0
             -0.0000000205303366552086916l0
             -0.0000000438915366654316784l0
             -0.0000000176840095080881795l0
             -0.0000000039912850280463420l0
             -0.0000000001869324124559212l0
             +0.0000000002729227396746077l0
             +0.0000000001328172131565497l0
             +0.0000000000318342484482286l0
             +0.0000000000016700607751926l0
             -0.0000000000020364649611537l0
             -0.0000000000009648468127965l0
             -0.0000000000002195672778128l0
             -0.0000000000000095689813014l0
             +0.0000000000000137032572230l0
             +0.0000000000000062538505417l0
             +0.0000000000000014584615266l0
             +0.0000000000000001078123993l0
             -0.0000000000000000706229988l0
             -0.0000000000000000391411775l0
             -0.0000000000000000111659209l0
             -0.0000000000000000015770366l0
             +0.0000000000000000002853149l0
             +0.0000000000000000002716662l0
             +0.0000000000000000000957770l0
             +0.0000000000000000000176835l0
             -0.0000000000000000000009828l0
             -0.0000000000000000000020464l0
             -0.0000000000000000000008020l0
             -0.0000000000000000000001650l0)))
    (loop for n upto 37 sum (* (elt l n) (chebyshev-polynomial n y)))))

(defun inverf (x)
  "Inverse error function."
  (let ((sign (if (minusp x) -1 1)) (x (abs x)))
    (cond
      ((< x 0.9975) (* sign (beta x) (r-function-a x)))
      ((> (- 1 x) 5l-16) (* sign (beta x) (r-function-b x)))
      (t (error "Inverse error function is not implemented for (1 - x) < 0.5e-16")))))

(defun quantile (p)
  "Calculates the quantile for the normal distribution."
  (* #.(sqrt 2) (inverf (- (* 2 p) 1))))

(defun confidence-interval (confidence-value &key two-sided)
  (if two-sided
      (quantile (/ (1+ confidence-value) 2))
      (quantile confidence-value)))
