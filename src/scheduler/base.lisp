(in-package :cl-rx.scheduler)

(defclass scheduler ()
  ()
  (:documentation "Base class for all schedulers"))

(defparameter *current-scheduler (make-instance 'immediate-scheduler))

(defgeneric schedule (scheduler fn)
  (:documentation "Schedules a new FN to be executed by the SCHEDULER"))

(defgeneric invoke-action (scheduler action)
  (:documentation "Executes the action in the current scheduler,
ACTION must be a function that takes no mandatory parameters"))

(defmacro with-current-scheduler (scheduler &body body)
  `(let ((,scheduler *current-scheduler*))
     ,@body))
