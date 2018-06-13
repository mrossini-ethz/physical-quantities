(defsystem "physical-quantities"
  :description "A library that provides a numeric type with optional unit and/or uncertainty for computations with automatic error propagation."
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
  :in-order-to ((test-op (test-op :physical-quantities/test))))

(defsystem "physical-quantities/test"
  :description "Unit testing for physical quantities."
  :author "Marco Rossini"
  :license "GPLv2"
  :depends-on (:physical-quantities)
  :pathname "test"
  :serial t
  :components ((:file "test-framework")
               (:file "test-init")
               (:module test-definitions
                        :pathname ""
                        :components ((:file "definition-test")
                                     (:file "conversion-test")
                                     (:file "operations-test")
                                     (:file "error-propagation-test")
                                     (:file "predicate-test")
                                     (:file "interface-test")
                                     (:file "namespace-test")))
               (:file "test")))

(defmethod perform ((operation test-op) (system (eql (find-system :physical-quantities/test))))
  (funcall (intern "PHYSICAL-QUANTITIES-TEST" :physical-quantities)))
