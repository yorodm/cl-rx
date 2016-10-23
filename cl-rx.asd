#|
  This file is a part of cl-rx project.
  Copyright (c) 2016 Yoandy Rodriguez Martinez
|#

#|
  Reactive extensions for Common-Lisp

  Author: Yoandy Rodriguez Martinez
|#

(in-package :cl-user)
(defpackage cl-rx-asd
  (:use :cl :asdf))
(in-package :cl-rx-asd)

(defsystem cl-rx
  :version "0.1"
  :author "Yoandy Rodriguez Martinez"
  :license "MIT"
  :depends-on ( :alexandria :bordeaux-threads)
  :serial t
  :components ((:module "src"
                        :serial t
                        :components
                        ((:module "subscriber"
                                  :serial t
                                  :components ((:file "package")
                                               (:file "subscriber")
                                               (:file "subscription")))
                         (:module "observable"
                                  :serial t
                                  :components((:file "package")
                                              (:file "operators")
                                              (:file "observable")))
                         (:module "util"
                                  :serial t
                                  :components((:file "package")))
                         (:module "scheduler"
                                  :serial t
                                  :components((:file "package")
                                              (:file "base")
                                              (:file "immediate")
                                              (:file "trampoline")))
                         (:file "package"))))
  :description "Reactive extensions for Common-Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-rx-test))))
