(in-package :cl-rx-test)


;; Testing the schedulers
(plan 2)
(subtest "Testing scheduling in the IMMEDIATE-SCHEDULER"
  (let ((*special-var* (list 5)))
  (with-current-scheduler (scheduler)
    (schedule scheduler #'(lambda ()
                            (push 20 *special-var*))))
  (is *special-var* (list 20 5))))

(setf *current-scheduler* (create-scheduler 'trampoline-scheduler))
(subtest "Testing scheduling in the TRAMPOLINE-SCHEDULER"
  (let ((*special-var* (list 5)))
    (with-current-scheduler (scheduler)
      (schedule  scheduler #'(lambda ()
                               (push 20 *special-var*))))
    (is *special-var* (list 20 5))))
;; Reset the scheduler
(setf *current-scheduler* (create-scheduler 'immediate-scheduler))
(finalize)
