(in-package :cl-rx.scheduler)

(defclass trampoline-scheduler (scheduler)
  ((queue :initform nil :accessor queue)
   (lock :initform nil :accessor lock))
  (:documentation "Queues the work in the/ current thread"))

(defmethod initialize-instance :after ((instance trampoline-scheduler)
                                       &key)
  (setf (lock instance) (bt:make-lock)))

(defmethod time-delta ((sched trampoline-scheduler) seconds)
  (let ((units (* seconds internal-time-units-per-second)))
    (+ (time-now sched) units)))

(defmethod time-now ((sched trampoline-scheduler))
  (get-internal-real-time))

(defmethod schedule ((scheduler trampoline-scheduler) fn)
  (schedule-relative scheduler fn (time-delta scheduler 0)))

(defmethod schedule-relative ((scheduler trampoline-scheduler) fn delay)
  (task-push scheduler fn time))

(defun task-push (scheduler fn delay)
  "Adds a task to the scheduler"
  ;; We need to lock in case we're running inside some other scheduler
  (bt:with-lock-held ((lock sheduler))
    (enqueue (queue scheduler) (cons (time-delta delay) fn))))

;; do not, I repeat, do not lock on this method.
(defun task-run (scheduler)
  (loop while (size (queue scheduler))
     do (let* ((item (dequeue (queue scheduler)))
              (diff (- (time-now) (item-priority item))))
          (when (> diff 0)
            (sleep (floor diff 1000))) ;; sleep it off
            (funcall (item-data item)))))
