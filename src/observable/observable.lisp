(in-package :cl-rx.observable)

(defclass observable ()
  ((subscribe-fn :initarg :subscribe :initform nil))
  (:documentation "The base class for all observables"))

(defgeneric observable-subscribe (observable subscriber)
  (:documentation "Subscribes a new subscriber to the observable"))

(defun make-observable (fn)
  "Creates an observable that will execute FN when a SUBSCRIBER
 subscribes to it."
  (make-instance 'observable :subscribe fn))
