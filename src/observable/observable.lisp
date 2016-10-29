(in-package :cl-rx.observable)

(defclass observable ()
  ((subscribe-fn :initarg :subscribe :initform nil :accessor subscribe-fn))
  (:documentation "The base class for all observables"))

(defgeneric observable-subscribe (observable subscriber)
  (:documentation "Subscribes a new subscriber to the observable"))

;; Go multi or go home
(defmethod observable-subscribe ((observable observable)
                                 (subscriber safe-subscriber))
  (funcall (subscribe-fn observable)  subscriber))

;; So we can call this using only the on-next function
(defmethod observable-subscribe ((observable observable)
                                 (subscriber function))
  (funcall (subscribe-fn observable) (make-subscriber :on-next subscriber)))
(defun make-observable (fn)
  "Creates an observable that will execute FN when a SUBSCRIBER
 subscribes to it."
  (make-instance 'observable :subscribe fn))
