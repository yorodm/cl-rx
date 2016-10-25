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

(defgeneric schedule-relative (scheduler fn time)
  (:documentation "Schedules a new FN to be executed after TIME seconds"))

(defmacro with-current-scheduler (scheduler &body body)
  `(let ((,scheduler *current-scheduler*))
     ,@body))
