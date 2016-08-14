(in-package :pq)

;; Unit factor structure

(defstruct (unit-factor (:conc-name uf-) (:constructor make-uf (unit power)))
  (unit "" :type string)
  (power 0 :type integer))

(defun uf-pow (unit-factor power)
  ;; Raises the unit factor to the given power
  (make-uf (uf-unit unit-factor) (* (uf-power unit-factor) power)))

(defun uf-equal (a b)
  ;; Tests for equality in the two unit factors by looking at both uf-unit and uf-power
  (and (equal (uf-unit a) (uf-unit b)) (= (uf-power a) (uf-power b))))
