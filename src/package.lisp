(in-package :cl-rx.util)
(macrolet
    ((package (package-name documentation &rest list)
       `(defpackage ,package-name
          (:documentation ,documentation)
          (:use #:cl ,@list)
          (:export
           ,@(loop for package in list
                   append (loop for symbol being the external-symbols in package
                                collect (make-symbol (string symbol))))))))
  (package #:cl-rx
"This is a convenience package which exports the external symbols of
 all other packages"
    #:cl-rx.subscriber
    #:cl-rx.scheduler
    #:cl-rx.observable
    #:cl-rx.util))
