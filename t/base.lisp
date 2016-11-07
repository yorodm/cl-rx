(in-package :cl-user)
(defpackage cl-rx-test
  (:use :cl
        :cl-rx.util-internal
        :cl-rx
        :prove))
(in-package :cl-rx-test)

;; Test all other things
(setf *current-scheduler* (create-scheduler 'immediate-scheduler))
(defparameter *source-list* (list 1 2 3 4 5 6 7 8 9))
(defparameter *source-list2* (list 2 4 6 8 10 12 14))
(defparameter *source-stream* (make-string-input-stream "This is the data"))
(defparameter *source-array* (make-array 5 :initial-contents '(1 2 3 4 5)))

(plan 1)
(subtest "Testing all the macros"
  (is-expand (-> 2 (+ 2) (* 4)) (* (+ 2 2) 4)))
(finalize)
