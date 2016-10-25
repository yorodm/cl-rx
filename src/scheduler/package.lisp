(in-package :cl-user)

(defpackage cl-rx.scheduler
  (:use :cl
        :cl-rx.util)

  (:export
   ;; Should I export this classes or just constructor functions?
   #:*current-scheduler*
   #:scheduler
   #:immediate-scheduler
   #:trampoline-scheduler
   #:schedule
   #:schedule-relative
   #:with-current-scheduler))
