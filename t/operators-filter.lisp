(in-package :cl-rx-test)


(plan 2)
(setf *current-scheduler* (create-scheduler 'immediate-scheduler))
(defparameter *source-list* (list 1 2 3 4 5 6 7 8 9))
(subtest "Testing the find operator"
  (let ((observ (-> (observable-from *source-list*)
                  (observable-find #'oddp)))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* '(9 7 5 3 1))))
(subtest "Testing the take operator"
  (let ((observ (-> (observable-from *source-list*)
                  (observable-take 3)))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* '(3 2 1))))
(finalize)
