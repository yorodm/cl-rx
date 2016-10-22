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
                          (cond ((null item) (suscriber-complete subs)
                                 (t (progn
                                      (suscriber-next subs item)
                                      (funcall callback state))))))))
               (with-current-scheduler (scheduler)
                 (schedule-recursive scheduler #'recursive)))))
    (make-observable #'from-list)))
