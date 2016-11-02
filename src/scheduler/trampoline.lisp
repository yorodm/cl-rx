(in-package :cl-rx.scheduler)

(defclass trampoline-scheduler (scheduler)
  ((queue :initform (make-instance 'pqueue) :accessor queue)
   (lock :initform (bt:make-lock) :accessor lock))
  (:documentation "Queues the work in the/ current thread"))

(defmethod time-delta ((sched trampoline-scheduler) seconds)
  (let ((units (* seconds internal-time-units-per-second)))
    (+ (time-now sched) units)))

(defmethod time-now ((sched trampoline-scheduler))
    (declare (ignore sched))
  (get-internal-real-time))

(defmethod schedule ((scheduler trampoline-scheduler) fn)
  (schedule-relative scheduler fn  0))

(defmethod schedule-relative ((scheduler trampoline-scheduler) fn delay)
  (task-push scheduler fn delay)
  (task-run scheduler))

(defun task-push (scheduler fn delay)
  "Adds a task to the scheduler"
  ;; We need to lock in case we're running inside some other scheduler
  (let ((delta (time-delta scheduler delay)))
  (bt:with-lock-held ((lock scheduler))
    (enqueue (queue scheduler) delta fn))))

;; do not, I repeat, do not lock on this method.
(defun task-run (scheduler)
  (loop while (> (size (queue scheduler)) 0)
     do (let* ((item (dequeue (queue scheduler)))
               (now (time-now scheduler))
               (diff (- (item-priority item) now)))
          (when (> diff 0)
            (sleep (floor diff 1000))) ;; sleep it off
            (funcall (item-data item)))))
