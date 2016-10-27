(in-package :cl-rx.scheduler)

(defclass scheduler ()
  ()
  (:documentation "Base class for all schedulers"))

(defvar *current-scheduler* nil "The current scheduler")


(defun create-scheduler (name)
  "Factory function, creates a new scheduler given its NAME as a symbol."
  (when (symbolp name)
  (make-instance name)))

(defgeneric schedule (scheduler fn)
  (:documentation "Schedules a new FN to be executed by the SCHEDULER"))

(defgeneric schedule-relative (scheduler fn delay)
  (:documentation "Schedules a new FN to be executed after TIME seconds"))

(defgeneric time-now (scheduler)
  (:documentation "Returns the current time according to SCHEDULER."))

(defgeneric time-delta (scheduler seconds)
  (:documentation "Calculates time deltas according to SCHEDULER"))

(defmacro with-current-scheduler (scheduler &body body)
  `(let ((,scheduler *current-scheduler*))
     ,@body))

(defmethod time-now ((scheduler scheduler))
  (warn "You should implement this method or inherit from someone who does")
  (values))

(defmethod time-delta ((scheduler scheduler) seconds)
  (warn "You should implement this method or inherit from someone who does")
  (values))
