(in-package :cl-rx.scheduler)

(defclass immediate-scheduler (scheduler)
  ()
  (:documentation "Executes the work immediately in the current thread"))

;; Method or function? that is the question
(defmethod invoke-action ((scheduler immediate-scheduler) action)
  (funcall action))

(defmethod schedule ((scheduler immediate-scheduler) fn)
  (invoke-action scheduler fn))

(defmethod schedule-relative ((scheduler immediate-scheduler) fn time)
  (warn "This operation will block the current thread")
  (sleep time)
  (schedule scheduler fn))
