(in-package :cl-rx-test)


(plan 3)
(setf *current-scheduler* (create-scheduler 'immediate-scheduler))
(defparameter *source-list* (list 1 2 3 4 5 6 7 8 9))
(defparameter *source-list2* (list 2 4 6 8 10 12 14))
(subtest "Testing the FILTER operator"
  (let ((observ (-> (observable-from *source-list*)
                  (observable-filter #'oddp)))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* '(9 7 5 3 1))))
(subtest "Testing the TAKE operator"
  (let ((observ (-> (observable-from *source-list*)
                  (observable-take 3)))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* '(3 2 1))))
(subtest "Testing the ALL operator"
  (let ((observ (-> (observable-from *source-list2*)
                  (observable-all #'evenp)))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x)
                                   ;; So we make sure only one value is generated
                                   (push x *special-var*)))
    (is *special-var* '(t))))
(finalize)
