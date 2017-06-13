;; Copyright (c) 2005, Peter Seibel All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.

;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.

;;     * Neither the name of the Peter Seibel nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :pq)

(defvar *test-name* nil)
(defvar *test-failures* 0)

(defmacro condition= (form condition)
  "Tests whether the execution of the form results in the given condition (returning T). If no condition or a different condition occurs, NIL is returned."
  `(handler-case (and ,form nil)
     (,condition () t)
     (t () nil)))

(defmacro define-test (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (when (null *test-name*)
       (setf *test-failures* 0))
     (let ((level (list-length *test-name*)) (*test-name* (append *test-name* (list ',name))) (test-failures-save *test-failures*))
       (format t "~V,0TTesting ~{~a~^:~} ...~%" level *test-name*)
       ,@body
       (if (> *test-failures* test-failures-save)
           (progn
             (format t "~V,0TTotal number of tests failed in ~{~a~^:~}: ~a~%" level *test-name* (- *test-failures* test-failures-save))
             (plusp level))
           t))))

(defun report-result (result form expanded-form)
  "Report the results of a single test case. Called by 'check'."
  (when (not result)
    (incf *test-failures*)
    (let ((*package* (find-package :pq)))
      (format t "~V,0T ~:[Failed~;Passed~]: ~s~@[ => ~*~s~]~%" (- (list-length *test-name*) 1) result form (not (equal form expanded-form)) expanded-form)))
  result)

(defmacro combine-results (&body forms)
  "Logical AND operation of the given forms, but without short-circuiting. This ensures that each form is evaluated exactly once."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  "Run each expression in 'forms' once and reports whether succeded (t) or failed (nil)."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f ,@(if (and (listp f) (not (eql 'condition= (first f)))) `((list ',(first f) ,@(rest f))) `(',f))))))

(defmacro check-with-side-effects (&body forms)
  "Run each expression in 'forms' once and reports whether succeded (t) or failed (nil)."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f ',f))))
