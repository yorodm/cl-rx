(in-package :cl-rx-test)


(plan 1)
(subtest "Testing the operator map"
  (let ((observ (-> (observable-from *source-list*)
                  (observable-map #'(lambda (x)
                                      (* 2 x)))))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* '(18 16 14 12 10 8 6 4 2))))
(finalize)
