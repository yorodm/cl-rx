(in-package :cl-rx.observable)

(defmethod observable-map ((observ observable) (fn function))
  (flet
      ((subscribe-fn (subs)
         (flet
             ((on-next (item)
                (subscriber-next subs (funcall fn item)))
              (on-completed ()
                (subscriber-completed subs))
              (on-error (cnd)
                (subscriber-error subs cnd)))
           (observable-subscribe observ
                                 (make-subscriber :on-next #'on-next
                                                  :on-error #'on-error
                                                  :on-completed #'on-completed)))))
           (make-observable #'subscribe-fn)))
