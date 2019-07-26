(in-package :cl-rx.scheduler)

;; Let's make this kind of TRAMPOLINE-SCHEDULER
(defclass new-thread-scheduler (trampoline-scheduler)
  ((ready-list :accessor ready-list
    ;; I don't want to risk sharing structure so.. no lists
    :initform (make-array 15 :fill-pointer 0 :adjustable t))
   ;; BT notification system
   (cvar :initform (bt:make-condition-variable))
   (thread :initform nil :accessor thread))
  (:documentation "Queues the work in a new thread"))


(defun isready (item)
  (with-current-scheduler (scheduler)
    (<= (- (item-priority item) (time-now scheduler)) 0)))

(defun event-loop ()
  "The event loop"
  (do () ;; infinite cicle
      (nil)
    (with-current-scheduler (scheduler)
      ;; move items ready to be executed into READY-LIST since I'm holding a
        ;; lock here and using a vector instead of a list I think this is pretty
        ;; much safe
      (bt:with-lock-held ((lock scheduler))
        ;; hold till we have something to move
        (bt:condition-wait (cvar scheduler) (lock scheduler))
        (when (and (> (size (queue scheduler)) 0)
                   (isready (top (queue scheduler))))
          (vector-push-extend (dequeue (queue scheduler))
                              (ready-list scheduler))))
      (when (length (ready-list scheduler))
        ))))

(defmethod scheduler-create ((scheduler new-thread-scheduler))
  ;; we have to rebind *CURRENT-SCHEDULER* for the new thread
  ;; so WITH-CURRENT-SCHEDULER knows how to find this one
  (let ((bt:*default-special-bindings*
         '((*current-scheduler* . scheduler))))
    (setf (thread scheduler) (bt:make-thread #'event-loop)))
  scheduler)

(defmethod task-run ((scheduler new-thread-scheduler))
  (bt:with-lock-held ((lock scheduler))
    ;; notify the loop
    (bt:condition-notify (cvar scheduler))))

(defmethod task-push ((scheduler new-thread-scheduler) fn delay)
  (let ((delta (time-delta scheduler delay)))
    (bt:with-lock-held ((lock scheduler))
      (if (<= delta (time-now sched))
          (vector-push-extend fn (ready-list scheduler)) ;; ready to run
          (enqueue (queue scheduler) fn delay)) ;; have to wait
      (task-run scheduler))))
