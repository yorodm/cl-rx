(in-package :cl-rx-test)

;; Testing the schedulers
(setf *current-scheduler* (create-scheduler 'immediate-scheduler))
(plan 1)
(subtest "Testing the IMMEDIATE-SCHEDULER"
  (let ((*special-var* (list 5)))
  (with-current-scheduler (scheduler)
    (schedule scheduler #'(lambda ()
                            (push 20 *special-var*))))
  (is *special-var* (list 20 5))))
(finalize)