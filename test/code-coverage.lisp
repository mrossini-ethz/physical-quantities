(require :sb-cover)

;; Turn on generation of code coverage instrumentation in the compiler
(declaim (optimize sb-cover:store-coverage-data))

(require :asdf)
;; Load the code, ensuring that it's recompiled with the new optimization policy.
(asdf:test-system :physical-quantities :force t)

;; Produce a coverage report
(sb-cover:report "/tmp/report/physical-quantities/")

;; Turn off instrumentation
(declaim (optimize (sb-cover:store-coverage-data 0)))
