(in-package :cl-rx.scheduler)

(defclass immediate-scheduler (scheduler)
  ()
  (:documentation "Executes the work immediately in the current thread"))

;; Method or function? that is the question
(defmethod invoke-action ((scheduler immediate-scheduler) action)
  (funcall action))

(defmethod schedule ((scheduler immediate-scheduler) fn)
  (invoke-action scheduler fn))

;;maybe this method is not a method but a function?
(defmethod schedule-recursive ((scheduler immediate-scheduler) fn)
  (warn "Recursive actions in the immediate scheduler can lead to stack overflows"))
