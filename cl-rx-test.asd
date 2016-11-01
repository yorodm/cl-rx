#|
  This file is a part of cl-rx project.
  Copyright (c) 2016 Yoandy Rodriguez Martinez
|#

(in-package :cl-user)
(defpackage cl-rx-test-asd
  (:use :cl :asdf))
(in-package :cl-rx-test-asd)

(defsystem cl-rx-test
  :author "Yoandy Rodriguez Martinez"
  :license "MIT"
  :depends-on (:cl-rx
               :prove)
  :components ((:module "t"
                :serial t
                :components
                ((:test-file "base")
                 (:test-file "pqueue")
                 (:test-file "schedulers")
                 (:test-file "operators-from"))))
  :description "Test system for cl-rx"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
