(in-package :cl-user)

(defpackage cl-rx.subscriber
  (:use :cl)
  (:export
   #:subscriber
   #:make-subscriber
   #:subscriber-next
   #:subscriber-completed
   #:subscriber-error))
