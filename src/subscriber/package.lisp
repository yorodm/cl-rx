(in-package :cl-user)

(defpackage cl-rx.subscriber
  (:use :cl)
  (:export
   #:subscriber
   #:subscription)
  (:import-from :alexandria :with-gensyms))
