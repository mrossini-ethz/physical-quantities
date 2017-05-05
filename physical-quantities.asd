(in-package :asdf-user)

(defsystem "physical-quantities"
  :description "Use lisp numbers for physical quantities with unit and error."
  :version "0.1"
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on ("parseq")
  :components ((:file "package")
               (:file "conditions")
               (:file "utils" :depends-on ("package"))
               (:file "parse-rules" :depends-on ("package"))
               (:file "unit-factor" :depends-on ("package"))
               (:file "quantity" :depends-on ("package"))
               (:file "unit-database" :depends-on ("package" "utils" "unit-factor"))
               (:file "units" :depends-on ("package" "utils" "quantity" "unit-factor" "unit-database"))
               (:file "numeric" :depends-on ("package" "utils" "quantity" "units" "conditions"))
               (:file "read-macro" :depends-on ("package" "utils" "quantity" "units"))
               (:file "si-units" :depends-on ("package" "unit-database" "read-macro" "parse-rules")))
  :in-order-to ((test-op (test-op :physical-quantities-test))))

(defsystem "physical-quantities-test"
  :description "Unit testing for physical quantities."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:physical-quantities)
  :components ((:file "test/test-framework")
               (:file "test/test" :depends-on ("test/test-framework"))))

(defmethod perform ((operation test-op) (system (eql (find-system :physical-quantities-test))))
  (funcall (intern "PHYSICAL-QUANTITIES-TEST" :physical-quantities)))
