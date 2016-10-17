(in-package :cl-rx.scheduler)

(defclass scheduler ()
  ()
  (:documentation "Base class for all schedulers"))

(defparameter *current-scheduler (make-instance 'immediate-scheduler))

(defgeneric schedule (scheduler fn)
  (:documentation "Schedules a new FN to be executed by the SCHEDULER"))

(defgeneric schedule-recursive(scheduler fn)
 (:documentation "Schedules FN to be executed recursively, FN recieve a single
 parameter. That parameter will be a function used to trigger the recursion "))

(defun invoke-action (fn)
  (fn *current-scheduler*))
