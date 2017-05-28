(in-package :asdf-user)

(defsystem "physical-quantities"
  :description "Use lisp numbers for physical quantities with unit and error."
  :version "0.1"
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on ("parseq")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "conditions")
               (:file "unit-factor")
               (:file "unit-database")
               (:file "units")
               (:file "quantity")
               (:file "numeric")
               (:file "parse-rules")
               (:file "read-macro")
               (:file "si-units"))
  :in-order-to ((test-op (test-op :physical-quantities-test))))

(defsystem "physical-quantities-test"
  :description "Unit testing for physical quantities."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:physical-quantities)
  :serial t
  :components ((:file "test/test-framework")
               (:file "test/test")))

(defmethod perform ((operation test-op) (system (eql (find-system :physical-quantities-test))))
  (funcall (intern "PHYSICAL-QUANTITIES-TEST" :physical-quantities)))
