(in-package :pq)

(define-test predicate-test ()
  (check
    ;; qequal real, real
    (qequal 3 3)
    (not (qequal 3 2))
    ;; qequal real, quantity
    (qequal 3 #q(3))
    (qequal #q(3) 3)
    (not (qequal #q(3 +/- 0 m) 3))
    (not (qequal 3 #q(3 +/- 0 m)))
    (not (qequal #q(3 +/- 1) 3))
    (not (qequal 3 #q(3 +/- 1)))
    ;; qequal quantity, quantity
    (qequal #q(3 +/- 2 m) #q(3 +/- 2 m))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 2.0 m)))
    (not (qequal #q(3 +/- 2 m) #q(300 +/- 200 cm)))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 3 m)))
    (not (qequal #q(3 +/- 2 m) #q(2 +/- 2 m)))
    (not (qequal #q(3 +/- 2 m) #q(3 +/- 2 s)))
    (not (qequal #q(1 newton) #q(1 kg m / s ^ 2)))
    (not (qequal #q(1 kelvin) #q(1 celsius)))

    ;; qequalp real, real
    (qequalp 3 3)
    (not (qequalp 3 2))
    ;; qequalp real, quantity
    (qequalp 3 #q(3))
    (qequalp #q(3) 3)
    (not (qequalp #q(3 +/- 0 m) 3))
    (not (qequalp 3 #q(3 +/- 0 m)))
    (not (qequalp #q(3 +/- 1) 3))
    (not (qequalp 3 #q(3 +/- 1)))
    ;; qequalp quantity, quantity
    (qequalp #q(3 +/- 2 m) #q(3 +/- 2 m))
    (qequalp #q(3 +/- 2 m) #q(3 +/- 2.0 m))
    (not (qequalp #q(3 +/- 2 m) #q(300 +/- 200 cm)))
    (not (qequalp #q(3 +/- 2 m) #q(3 +/- 3 m)))
    (not (qequalp #q(3 +/- 2 m) #q(2 +/- 2 m)))
    (not (qequalp #q(3 +/- 2 m) #q(3 +/- 2 s)))
    (qequalp #q(1 newton) #q(1 kg m / s ^ 2))
    (qequalp #q(1 kelvin) #q(1 celsius))

    ;; q= real, real
    (q= 3 3)
    (not (q= 3 2))
    ;; q= real, quantity
    (q= #q(3) 3)
    (q= 3 #q(3))
    (not (q= #q(3) 2))
    (not (q= 2 #q(3)))
    (condition= (q= #q(3 m) 3) invalid-unit-conversion-error)
    (condition= (q= 3 #q(3 m)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 25 to 28 never (q= #q(30 +/- 1) (+ v 0.035))))
    (identity (loop for v upfrom 28 to 30 always (q= #q(30 +/- 1) (+ v 0.045))))
    (identity (loop for v upfrom 30 to 32 always (q= #q(30 +/- 1) (- v 0.045))))
    (identity (loop for v upfrom 32 to 35 never (q= #q(30 +/- 1) (- v 0.035))))
    ;; q= quantity, quantity
    (q= #q(3) #q(3))
    (not (q= #q(3) #q(4)))
    (condition= (q= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (not (q= #q(30 +/- 2 m) #q(21 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(22 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(23 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(24 +/- 2 m)))
    (q= #q(30 +/- 2 m) #q(25 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(26 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(27 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(28 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(29 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(30 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(31 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(32 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(33 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(34 +/- 2 m))
    (q= #q(30 +/- 2 m) #q(35 +/- 2 m))
    (not (q= #q(30 +/- 2 m) #q(36 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(37 +/- 2 m)))
    (not (q= #q(30 +/- 2 m) #q(38 +/- 2 m)))
    (not (q= #q(30 +/- 3 m) #q(39 +/- 3 m)))

    ;; q/=
    (q/= #q(30 +/- 1 m) #q(25.61 +/- 2 m))
    (not (q/= #q(30 +/- 1 m) #q(25.62 +/- 2 m)))
    (not (q/= #q(30 +/- 1 m) #q(34.38 +/- 2 m)))
    (q/= #q(30 +/- 1 m) #q(34.39 +/- 2 m))

    ;; q< real, real
    (q< 2 3)
    (not (q< 2 2))
    (not (q< 3 2))
    ;; q< quantity, real
    (condition= (q< #q(3 m) 3) invalid-unit-conversion-error)
    (q< #q(3) 4)
    (not (q< #q(3) 3))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q< q (+ v 0.64))))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q< q (+ v 0.65))))
    ;; q< real, quantity
    (condition= (q< 3 #q(3 m)) invalid-unit-conversion-error)
    (q< 3 #q(4))
    (not (q< 3 #q(3)))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q< (- v 0.65) q)))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q< (- v 0.64) q)))
    ;; q< quantity, quantity
    (condition= (q< #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 203 with q = #q(200 +/- 2) never (q< q #q((+ V 0.667) +/- 1))))
    (identity (loop for v upfrom 203 to 210 with q = #q(200 +/- 2) always (q< q #q((+ V 0.689) +/- 1))))

    ;; q<= real, real
    (q<= 2 3)
    (q<= 2 2)
    (not (q<= 3 2))
    ;; q<= quantity, real
    (condition= (q<= #q(3 m) 3) invalid-unit-conversion-error)
    (q<= #q(3) 4)
    (q<= #q(3) 3)
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q<= q (+ v 0.64))))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q<= q (+ v 0.65))))
    ;; q<= real, quantity
    (condition= (q<= 3 #q(3 m)) invalid-unit-conversion-error)
    (q<= 3 #q(4))
    (q<= 3 #q(3))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q<= (- v 0.65) q)))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q<= (- v 0.64) q)))
    ;; q<= quantity, quantity
    (condition= (q<= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 203 with q = #q(200 +/- 2) never (q<= q #q((+ V 0.667) +/- 1))))
    (identity (loop for v upfrom 203 to 210 with q = #q(200 +/- 2) always (q<= q #q((+ V 0.689) +/- 1))))

    ;; q> real, real
    (q> 3 2)
    (not (q> 2 2))
    (not (q> 2 3))
    ;; q> quantity, real
    (condition= (q> #q(3 m) 3) invalid-unit-conversion-error)
    (q> #q(4) 3)
    (not (q> #q(3) 3))
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q> q (- v 0.65))))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q> q (- v 0.64))))
    ;; q> real, quantity
    (condition= (q> 3 #q(3 m)) invalid-unit-conversion-error)
    (q> 4 #q(3))
    (not (q> 3 #q(3)))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q> (+ v 0.64) q)))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q> (+ v 0.65) q)))
    ;; q> quantity, quantity
    (condition= (q> #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 197 with q = #q(200 +/- 2) always (q> q #q((- V 0.689) +/- 1))))
    (identity (loop for v upfrom 197 to 210 with q = #q(200 +/- 2) never (q> q #q((- V 0.667) +/- 1))))

    ;; q>= real, real
    (q>= 3 2)
    (q>= 2 2)
    (not (q>= 2 3))
    ;; q>= quantity, real
    (condition= (q>= #q(3 m) 3) invalid-unit-conversion-error)
    (q>= #q(4) 3)
    (q>= #q(3) 3)
    (identity (loop for v upfrom 195 to 199 with q = #q(200 +/- 1) always (q>= q (- v 0.65))))
    (identity (loop for v upfrom 199 to 205 with q = #q(200 +/- 1) never (q>= q (- v 0.64))))
    ;; q>= real, quantity
    (condition= (q>= 3 #q(3 m)) invalid-unit-conversion-error)
    (q>= 4 #q(3))
    (q>= 3 #q(3))
    (identity (loop for v upfrom 195 to 201 with q = #q(200 +/- 1) never (q>= (+ v 0.64) q)))
    (identity (loop for v upfrom 201 to 205 with q = #q(200 +/- 1) always (q>= (+ v 0.65) q)))
    ;; q>= quantity, quantity
    (condition= (q>= #q(3 m) #q(3 s)) invalid-unit-conversion-error)
    (identity (loop for v upfrom 190 to 197 with q = #q(200 +/- 2) always (q>= q #q((- V 0.689) +/- 1))))
    (identity (loop for v upfrom 197 to 210 with q = #q(200 +/- 2) never (q>= q #q((- V 0.667) +/- 1))))

    ;; units-equal
    (units-equal (mkunit |m| / |s|) (mkunit / |s| |m|))
    (not (units-equal (mkunit |km| / |h|) (mkunit |m| / |s|)))
    (units-equal (mkunit |m| ^ 2 / |s| / |m|) (mkunit |m| / |s|))
    (not (units-equal (mkunit |newton|) (mkunit |kg| |m| / |s| ^ 2)))
    ;; units-equalp
    (units-equalp (mkunit |newton|) (mkunit |m| |kg| / |s| ^ 2))
    (not (units-equalp (mkunit |km| / |h|) (mkunit |m| / |s|)))
    ;; units-convertible
    (units-convertible (mkunit |km| / |h|) (mkunit |m| / |s|))
    (units-convertible (mkunit |kW| |h|) (mkunit |joule|))
    (not (units-convertible (mkunit |km| / |h|) (mkunit |m| / |s| ^ 2)))))
