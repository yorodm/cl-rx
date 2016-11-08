(in-package :cl-rx.observable)

;; There are two ways we can go here: We can either loop over the whole thing
;; calling SUBSCRIBER-NEXT or we can provide a safer way by providing a callback
;; to "recursively" generate events. LOOP seems like a poor choice given that I
;; don't know if it will be thread safe. Maybe I can set a lot of locks but that
;; might have an impact in performance.
(defmethod observable-from ((source list))
  (flet ((from-list (subs) ;; this is the subscribe function
           (flet ((producer () ;; this will be called by the scheduler
                    (protect-with (subs error)
                             (loop for item in source
                                do (subscriber-next subs item))
                             (subscriber-completed subs))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-list)))

;; Yeah, different words in loop so...
(defmethod observable-from ((source array))
  (flet ((from-list (subs) ;; this is the subscribe function
           (flet ((producer () ;; this will be called by the scheduler
                    (protect-with (subs error)
                                        (loop for item across source
                                           do (subscriber-next subs item))
                                        (subscriber-completed subs))))
               (with-current-scheduler (scheduler)/
                 (schedule scheduler #'producer)))))
    (make-observable #'from-list)))


;; Since gray streams are not part of the standard we have to provide to
;; different methods. One will handle the whole "standard stream types"
;; situation like STRING-STREAM and TWO-WAY-STREAMS. The other will handle gray
;; streams since they're in almost every implementation anyway.

(defmacro read-stream (source eof-error-p &optional eof-value)
  "Reads from a stream"
  `(if (subtypep 'character (stream-element-type ,source))
      (read-char ,source ,eof-error-p ,eof-value)
      (read-byte ,source ,eof-error-p ,eof-value)))

(defmethod observable-from ((source stream))
  (assert (input-stream-p source))
  (flet ((from-stream (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                      (protect-with (subs error)
                                    (loop for byte = (read-stream source nil)
                                       while byte
                                       do (subscriber-next subs byte))
                                    (subscriber-completed subs))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-stream)))

(defmethod observable-from ((source fundamental-input-stream))
  (flet ((from-stream (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                      (protect-with (subs error)
                                    (loop for byte = (read-stream source nil)
                                       while byte
                                       do (subscriber-next subs byte))
                                    (subscriber-completed subs))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-stream)))

(defmethod observable-just ((source t))
  (flet ((just (subs) ;; this is the subscribe function
           (flet
               ((producer () ;; this will be called by the scheduler
                  (protect-with (subs error)
                  (subscriber-next subs source)
                  (subscriber-completed subs))))
             (with-current-scheduler (scheduler)
               (schedule scheduler #'producer)))))
    (make-observable #'just)))
