(in-package :cl-rx-test)


(plan 3)

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
