(in-package :cl-rx.observable)

(defmethod observable-find ((observ observable) (predicate function))
  (flet
      ((subscribe-fn (subs)
         (flet
             ((on-next (item)
                (when (funcall predicate item)
                  (subscriber-next subs item)))
              (on-completed ()
                (subscriber-completed subs))
              (on-error (cnd)
                (subscriber-error subs cnd)))
           (observable-subscribe observ
                                 (make-subscriber :on-next #'on-next
                                                  :on-error #'on-error
                                                  :on-completed #'on-completed)))))
    (make-observable #'subscribe-fn)))

(defmethod observable-take ((observ observable) (n integer))
  (flet
      ((subscribe-fn (subs)
         (let ((counter 0))
           (flet
               ((on-next (item)
                  (if (< counter n)
                      (progn
                        (subscriber-next subs item)
                        (incf counter))
                      (subscriber-completed subs)))
                (on-completed ()
                  (subscriber-completed subs))
                (on-error (cnd)
                  (subscriber-error subs cnd)))
             (observable-subscribe observ
                                   (make-subscriber :on-next #'on-next
                                                    :on-error #'on-error
                                                    :on-completed #'on-completed))))))
    (make-observable #'subscribe-fn)))

(defmethod observable-first ((observ observable))
  (observable-take observ 1))

(defmethod observable-all ((observ observable) (predicate function))
  (flet
      ((subscribe-fn (subs)
         (let ((result t))
           (flet ((on-next (item)
                    (setf result (and (funcall predicate item) result)))
                  (on-completed ()
                    (subscriber-next subs result)
                    (subscriber-completed subs))
                  (on-error (cnd)
                    (subscriber-error subs cnd)))
             (observable-subscribe observ
                                   (make-subscriber :on-next #'on-next
                                                    :on-error #'on-error
                                                    :on-completed #'on-completed))))))
        (make-observable #'subscribe-fn)))
