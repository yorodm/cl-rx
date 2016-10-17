(in-package :cl-rx.observable)


(defgeneric observable-from(source)
  (:documentation "Creates a new observable from the given source"))


;; There are two ways we can go here:
;; 1) The RxJava way when we set producers and
;; 2) The RxPython
(defmethod observable-from ((source list))
  (labels ((from-list (subs) ;; this is the subscribe function
             (labels ((recursive (callback state)
                        (let ((item (pop source)))
                          (cond ((null item) (progn
                                               (suscriber-complete subs)
                                               (funcall callback #'recursive))
                                 (t (suscriber-next subs item)))))))
               (with-current-scheduler (scheduler)
                 (schedule-recursive #'recursive)))))
    (make-observable #'from-list)))
