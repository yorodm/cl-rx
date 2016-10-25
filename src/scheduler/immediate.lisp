(in-package :cl-rx.scheduler)

(defclass immediate-scheduler (scheduler)
  ()
  (:documentation "Executes the work immediately in the current thread"))

(defmethod schedule ((scheduler immediate-scheduler) fn)
  (funcall fn))

(defmethod schedule-relative ((scheduler immediate-scheduler) fn time)
  (warn "This operation will block the current thread")
  (sleep time)
  (schedule scheduler fn))
