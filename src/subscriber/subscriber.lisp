(in-package :cl-rx.subscriber)

(defun noop (&rest others)
  "Null operation"
  (declare (ignore others))
  (values))

(defclass subscriber ()
  ((on-next
    :initarg :on-next
    :accessor on-next
    :documentation "Called when the observable generates a new value")
   (on-error
    :initarg :on-error
    :accessor on-error
    :documentation "Called when the observable finds an error")
   (on-completed
    :initarg :on-completed
    :accessor on-completed
    :documentation "Called when the observable finishes processing values"))
  (:documentation "A subscriber"))

(defclass safe-subscriber (subscriber) ;; this is internal
  ((inner :initarg :inner :accessor inner)
   (state :initarg :state :accessor state))
  (:documentation "A class to wrap user created subscribers"))

(defmethod make-load-form ((self subscriber) &optional env)
  (make-load-form-saving-slots self
                               :slot-names '(on-next on-error on-completed)
                               :environment env))

(defmethod make-load-form ((self safe-subscriber) &optional env)
  (make-load-form-saving-slots self
                               :slot-names '(state inner on-next on-error on-completed)
                               :environment env))

(defun safe-subscriber (wrapped)
  ;; this is basically a state machine
  "Wraps SUB into a safe subscriber. The safe subscriber guarantees that
SUB behaves properly"
  (flet ((on-next (sub it)
           (case (state sub)
             ('live (subscriber-next (inner sub) it))
             (otherwise (values))))
         (on-error (sub cnd)
           (case (state sub)
             ('live (setf (state sub) 'error)
                    (subscriber-error (inner sub) cnd))
             (otherwise (values))))
         (on-complete (sub)
           (case (state sub)
             ('live (setf (state sub) 'completed)
                    (subscriber-completed (inner sub)))
             (otherwise (values)))))
    (make-instance 'safe-subscriber
                   :state 'live
                   :inner wrapped
                   :on-next #'on-next
                   :on-error #'on-error
                   :on-completed #'on-complete)))

;; public interface

(defun make-subscriber (&key (on-next #'noop)
                             (on-completed #'noop)
                             (on-error #'noop))
          "Creates a new subscriber given at least one of it functions. The arguments are:
ON-NEXT: Notifies the SUBSCRIBER of new events in the OBSERVABLE. Takes the item as an argument.
ON-ERROR: Notifies the SUBSCRIBER of errors in the OBSERVABLE. Takes the error as an argument.
ON-COMPLETED: Notifies the SUBSCRIBER that the OBSERVABLE will no longer send events it's way.

This function is the only safe way to create a subscriber so do not call make-instance."

          (safe-subscriber (make-instance 'subscriber :on-next on-next
                                          :on-completed on-completed
                                          :on-error on-error)))

(defgeneric subscriber-next (sub item))

(defgeneric subscriber-completed (sub))

(defgeneric subscriber-error (sub cond))

(defmethod subscriber-next ((sub subscriber) item)
  (funcall (on-next sub) item))

(defmethod subscriber-completed ((sub subscriber))
  (funcall (on-completed sub)))

(defmethod subscriber-error ((sub subscriber) cond)
  (funcall (on-error sub) cond))

(defmethod subscriber-next ((sub safe-subscriber) item)
  (funcall (on-next sub) sub item))

(defmethod subscriber-completed ((sub safe-subscriber))
  (funcall (on-completed sub) sub))

(defmethod subscriber-error ((sub safe-subscriber) cond)
  (funcall (on-error sub) sub cond))
