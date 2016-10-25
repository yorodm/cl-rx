(in-package :cl-rx.observable)

;; There are two ways we can go here:
;; We can either loop over the whole thing calling SUBSCRIBER-NEXT or
;; we can provide a safer way by providing a callback to "recursively"
;; generate events. LOOP seems like a poor choice given that I don't
;; know if it will be thread safe. Maybe I can set a lot of locks but
;; that might have an impact in performance.
(defmethod observable-from ((source list))
  (labels ((from-list (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                        (unwind-protect (progn ;; maybe handler-case?
                                          (loop for item in source
                                             do (subscriber-next subs item))
                                          (subscriber-completed subs))
                          ;; Call on-error as cleanup.
                          ;; see CL-RX.SUBSCRIBER:SAFE-SUBSCRIBER
                          (subscriber-error subs))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-list)))
