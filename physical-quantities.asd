(in-package :asdf-user)

(defsystem "physical-quantities"
  :description "Use lisp numbers for physical quantities with unit and error."
  :version "0.1"
  :author "Marco Rossini"
  :depends-on ("parser")
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "unit-factor" :depends-on ("package"))
               (:file "parse-rules" :depends-on ("package" "unit-factor"))
               (:file "quantity" :depends-on ("package"))
               (:file "units" :depends-on ("package" "utils" "parse-rules" "quantity" "unit-factor"))
               (:file "si-units" :depends-on ("package" "utils" "parse-rules" "quantity" "units"))
               (:file "numeric" :depends-on ("package" "units" "utils"))
               (:file "read-macro" :depends-on ("package" "quantity" "parse-rules"))))
