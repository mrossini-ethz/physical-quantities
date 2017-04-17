(in-package :asdf-user)

(defsystem "physical-quantities"
  :description "Use lisp numbers for physical quantities with unit and error."
  :version "0.1"
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on ("parseq")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "unit-factor" :depends-on ("package"))
               (:file "parse-rules" :depends-on ("package" "unit-factor"))
               (:file "quantity" :depends-on ("package"))
               (:file "units" :depends-on ("package" "utils" "parse-rules" "quantity" "unit-factor"))
               (:file "si-units" :depends-on ("package" "utils" "parse-rules" "quantity" "units"))
               (:file "numeric" :depends-on ("package" "units" "utils"))
               (:file "read-macro" :depends-on ("package" "quantity" "parse-rules")))
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
