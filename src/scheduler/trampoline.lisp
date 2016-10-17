(in-package :cl-rx.scheduler)

(defclass trampoline-scheduler (scheduler)
  ((queue :initarg :queue :initform nil :acces))
  (:documentation "Queues the work in the current thread"))

(defmethod schedule ((scheduler tampoline-scheduler) fn))
