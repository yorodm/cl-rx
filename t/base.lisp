(in-package :cl-user)
(defpackage cl-rx-test
  (:use :cl
        :cl-rx.util-internal
        :cl-rx
        :prove))
(in-package :cl-rx-test)

;; Test all other things

(plan 1)
(subtest "Testing all the macros"
  (is-expand (-> 2 (+ 2) (* 4)) (* (+ 2 2) 4)))
(finalize)
