(in-package :cl-rx.scheduler)

(defclass scheduler ()
  ()
  (:documentation "Base class for all schedulers"))

(defun create-scheduler (name) ;; this will soon be a macro
  "Factory function, creates a new scheduler given its NAME as a symbol."
  (when (symbolp name)
    (make-instance name)))

(defvar *current-scheduler* nil "The current scheduler")
;; (load-time-value (create-scheduler 'trampoline-scheduler)) "The current scheduler")

(defgeneric schedule (scheduler fn)
  (:documentation "Schedules a new FN to be executed by the SCHEDULER"))

(defgeneric schedule-relative (scheduler fn delay)
  (:documentation "Schedules a new FN to be executed after TIME seconds"))

(defgeneric time-now (scheduler)
  (:documentation "Returns the current time according to SCHEDULER."))

(defgeneric time-delta (scheduler seconds)
  (:documentation "Calculates time deltas according to SCHEDULER"))

(defmacro with-current-scheduler ((var) &body body)
  "Executes BODY with VAR bound to the current scheduler"
  `(let ((,var *current-scheduler*))
     ,@body))

(defmethod time-now ((scheduler scheduler))
  (declare (ignore scheduler))
  (warn "You should implement this method or inherit from someone who does")
  (values))

(defmethod time-delta ((scheduler scheduler) seconds)
    (declare (ignore scheduler seconds))
  (warn "You should implement this method or inherit from someone who does")
  (values))
