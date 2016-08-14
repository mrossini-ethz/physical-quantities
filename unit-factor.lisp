(in-package :pq)

;; Unit factor structure

(defstruct (unit-factor (:conc-name uf-) (:constructor make-uf (unit power)))
  (unit "" :type string)
  (power 0 :type integer))

(defun uf-pow (unit-factor power)
  (make-uf (uf-unit unit-factor) (* (uf-power unit-factor) power)))

(defun uf-same (a b)
  (equal (uf-unit a) (uf-unit b)))

(defun uf-equal (a b)
  (and (equal (uf-unit a) (uf-unit b)) (= (uf-power a) (uf-power b))))
