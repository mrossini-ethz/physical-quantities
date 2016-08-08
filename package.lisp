(require :asdf)
(asdf:load-system :parser)

(defpackage :physical-quantities
  (:use :common-lisp :parser)
  (:nicknames :physq :pquant :pq))
