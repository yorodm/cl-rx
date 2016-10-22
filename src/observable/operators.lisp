(in-package :cl-rx.observable)


(defgeneric observable-from(source)
  (:documentation "Creates a new observable from the given source"))


;; There are two ways we can go here:
;; We can either loop over the whole thing calling SUBSCRIBER-NEXT or
;; we can provide a safer way by providing a callback to "recursively"
;; generate events. LOOP seems like a poor choice given that I don't
;; know if it will be thread safe. Maybe I can set a lot of locks but
;; that might have an impact in performance.
(defmethod observable-from ((source list))
  (labels ((from-list (subs) ;; this is the subscribe function
             (labels ((recursive (callback state)
                        ;; is pop safe for threading?
                        ;; I should lock on this
                        (let ((item (pop source)))
                          (cond ((null item) (suscriber-complete subs)
                                 (t (progn
                                      (suscriber-next subs item)
                                      (funcall callback state))))))))
               (with-current-scheduler (scheduler)
                 (schedule-recursive scheduler #'recursive)))))
    (make-observable #'from-list)))
