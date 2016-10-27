(in-package :cl-user)

(defpackage cl-rx.util-internal
  (:use :cl)
  (:export #:pqueue
           #:enqueue
           #:dequeue
           #:top))

(defpackage cl-rx.util
  (:use :cl))

(in-package :cl-rx.util)
