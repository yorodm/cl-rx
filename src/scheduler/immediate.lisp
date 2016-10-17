(in-package :cl-rx.scheduler)

(defclass immediate-scheduler (scheduler)
  ()
  (:documentation "Executes the work immediately in the current thread"))

(defmethod schedule)
