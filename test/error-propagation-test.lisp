(in-package :pq)

(define-test error-propagation-test ()
  (check
    ;; Q+
    (qtest (q+ #q(1 +/- 0.3) #q(-2 +/- 0.5) #q(3 +/- 0.7)) :value 2 :error (py+ 0.3 0.5 0.7) :unit ())
    ;; Q-
    (qtest (q- #q(1 +/- 0.3) #q(-2 +/- 0.5) #q(3 +/- 0.7)) :value 0 :error (py+ 0.3 0.5 0.7) :unit ())
    ;; Q*
    (qtest (q* #q(2 +/- 0.1d0) #q(-6 +/- 0.2d0) #q(8 +/- 0.2d0)) :value -96 :error (* 96 (py+ 0.05d0 2/60 0.025d0)) :unit ())
    ;; Q/
    (qtest (q/ #q(2 +/- 0.1d0) #q(-6 +/- 0.2d0) #q(8 +/- 0.2d0)) :value -2/48 :error (* 2/48 (py+ 0.05d0 2/60 0.025d0)) :unit ())
    ;; QPOW
    (qtest (qpow #q(-3 +/- 0.1d0 m) -3) :value -1/27 :error (* 1/27 0.1d0) :unit '(("m" -3)))
    (qtest (qpow #q(-3 +/- 0.1d0 m) 0) :value 1 :error 0 :unit '())
    (qtest (qpow #q(-3 +/- 0.1d0 m) 1) :value -3 :error 0.1d0 :unit '(("m" 1)))
    (qtest (qpow #q(-3 +/- 0.1d0 m) 3) :value -27 :error 2.7d0 :unit '(("m" 3)))
    ;; QROOT
    (qtest (qroot #q(16d0 +/- 0.2d0 m ^ 2) 2) :value 4.0d0 :error 0.025d0 :unit '(("m" 1)))
    (qtest (qroot #q(-27d0 +/- 0.2d0 m ^ 3) 3) :value (- (expt 27d0 1/3)) :error (* 0.2d0 1/27) :unit '(("m" 1)))
    ;; QEXP
    (qtest (qexp #q(-3d0 +/- 0.1d0)) :value (exp -3d0) :error (* 0.1d0 (exp -3d0)) :unit ())
    ;; QEXPT
    (qtest (qexpt #q(3d0 +/- 0.1d0) #q(2d0 +/- 0.2d0)) :value (expt 3d0 2d0) :error (py+ 0.6d0 (/ 1.8d0 (log 3d0))) :unit ())
    (qtest (qexpt #q(3d0 +/- 0.1d0) #q(0d0 +/- 0.2d0)) :value (expt 3d0 0d0) :error (/ 0.2d0 (log 3d0)) :unit ())
    (qtest (qexpt #q(3d0 +/- 0.1d0) #q(-2d0 +/- 0.2d0)) :value (expt 3d0 -2d0) :error (py+ (/ 0.2d0 27d0) (/ 0.2d0 9d0 (log 3d0))) :unit ())
    ;; QLN
    (qtest (qln #q(3d0 +/- 0.1d0)) :value (log 3d0) :error (/ 0.1d0 3d0) :unit ())
    ;; QLOG
    (qtest (qlog #q(100d0 +/- 0.5d0) #q(10d0 +/- 0.1d0)) :value 2d0 :error (py+ (/ (* 0.1d0 (log 100d0)) 10d0 (expt (log 10d0) 2)) (/ 0.5d0 100d0 (log 10d0))) :unit ())
    ;; QSIN
    (qtest (qsin #q(0.5d0 +/- 0.1d0)) :value (sin 0.5d0) :error (* 0.1d0 (cos 0.5d0)) :unit ())
    ;; QCOS
    (qtest (qcos #q(0.5d0 +/- 0.1d0)) :value (cos 0.5d0) :error (* 0.1d0 (sin 0.5d0)) :unit ())
    ;; QTAN
    (qtest (qtan #q(0.5d0 +/- 0.1d0)) :value (tan 0.5d0) :error (/ 0.1d0 (expt (cos 0.5d0) 2)) :unit ())
    ;; QASIN
    (qtest (qasin #q(0.5d0 +/- 0.1d0)) :value (asin 0.5d0) :error (* 0.1d0 (/ (sqrt 0.75d0))) :unit ())
    ;; QACOS
    (qtest (qacos #q(0.5d0 +/- 0.1d0)) :value (acos 0.5d0) :error (* 0.1d0 (/ (sqrt 0.75d0))) :unit ())
    ;; QATAN
    (qtest (qatan #q(0.5d0 +/- 0.1d0)) :value (atan 0.5d0) :error (* 0.1d0 (/ 1.25d0)) :unit ())
    ;; QSINH
    (qtest (qsinh #q(0.5d0 +/- 0.1d0)) :value (sinh 0.5d0) :error (* 0.1d0 (cosh 0.5d0)) :unit ())
    ;; QCOSH
    (qtest (qcosh #q(0.5d0 +/- 0.1d0)) :value (cosh 0.5d0) :error (* 0.1d0 (sinh 0.5d0)) :unit ())
    ;; QTANH
    (qtest (qtanh #q(0.5d0 +/- 0.1d0)) :value (tanh 0.5d0) :error (/ 0.1d0 (expt (cosh 0.5d0) 2)) :unit ())
    ;; QASINH
    (qtest (qasinh #q(0.5d0 +/- 0.1d0)) :value (asinh 0.5d0) :error (* 0.1d0 (/ (sqrt 1.25d0))) :unit ())
    ;; QACOSH
    (qtest (qacosh #q(1.5d0 +/- 0.1d0)) :value (acosh 1.5d0) :error (* 0.1d0 (/ (sqrt 1.25d0))) :unit ())
    ;; QATANH
    (qtest (qatanh #q(0.5d0 +/- 0.1d0)) :value (atanh 0.5d0) :error (* 0.1d0 (/ 0.75d0)) :unit ())
    ;; QABS
    (qtest (qabs #q(0.5d0 +/- 0.1d0)) :value 0.5d0 :error 0.1d0 :unit ())
    (qtest (qabs #q(-0.5d0 +/- 0.1d0)) :value 0.5d0 :error 0.1d0 :unit ())))
