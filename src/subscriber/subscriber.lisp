(in-package :cl-rx.subscriber)

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
    :init-arg :on-completed
    :accessor on-completed
    :documentation "Called when the observable finishes processing values"))
  (:documentation "A subscriber"))

(defmacro defsubscriber (name &rest body))

(defclass safe-subscriber (subscriber) ;; this is internal
  ((inner :initarg :inner :accesor inner)
   (state :initarg :state :accespr state :initform :live))
  (:documentation "A class to wrap user created subscriber"))

(defun parse-body (body)
  "The old parse-body, it makes sure that body has the form is suitable for
defining a new function using flet"
  ())

(defun parse-body (body &key documentation whole)
  "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
  (let ((doc nil)
        (decls nil)
        (current nil))
    (tagbody
     :declarations
       (setf current (car body))
       (when (and documentation (stringp current) (cdr body))
         (if doc
             (error "Too many documentation strings in ~S." (or whole body))
             (setf doc (pop body)))
         (go :declarations))
       (when (and (listp current) (eql (first current) 'declare))
         (push (pop body) decls)
         (go :declarations)))
    (values body (nreverse decls) doc)))

(defmacro make-subscriber &key on-next on-completed on-error
          "Creates a new subscriber given at least one of it functions. The parameters are:
ON-NEXT: A function that takes two parameters. The SUBSCRIBER and the item generated
by the OBSERVABLE.
ON-ERROR: A function that takes two parameters, the SUBSCRIBER and a function receiving
the condition as a parameter.
ON-COMPLETED: A function that takes a SUBSCRIBER as a parameter.
This function is the only safe way to create a subscriber so do not call make-instance."

          (safe-subscriber (make-instance 'subscriber :on-next on-next
                                          :on-completed on-completed
                                          :on-error on-error)))

(defun safe-subscriber (inner)
  ;; this is basically a state machine
  "Wraps SUB into a safe subscriber. The safe subscriber guarantees that
SUB behaves acordingly to observable semantics"
  (flet ((on-next (sub it)
           (case ()
               ))
         (on-error (cnd) (values))
         (on-complete (sub)))
    (make-instance 'safe-subscriber
                   :inner inner
                   :on-next #'on-next
                   :on-error #'on-error
                   :on-complete #'on-complete)))

;; Need generic functions to cover the case of safe subscriber

(defmethod subscriber-next ((sub subscriber) item)
  (funcall (on-next sub) item))

(defmethod subscriber-completed ((sub subscriber))
  (funcall (on-completed sub)))

(defmethod subscriber-error ((sub subscriber) cond)
  (funcall (on-error sub) cond))
