(in-package :cl-user)

(defpackage cl-rx.scheduler
  (:use :cl
        :cl-rx.util-internal)
  (:export
   #:*current-scheduler*
   #:trampoline-scheduler
   #:immediate-scheduler
   #:create-scheduler
   #:schedule
   #:schedule-relative
   #:with-current-scheduler))
