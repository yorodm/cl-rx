(in-package :cl-rx.observable)

;; There are two ways we can go here: We can either loop over the whole thing
;; calling SUBSCRIBER-NEXT or we can provide a safer way by providing a callback
;; to "recursively" generate events. LOOP seems like a poor choice given that I
;; don't know if it will be thread safe. Maybe I can set a lot of locks but that
;; might have an impact in performance.
(defmethod observable-from ((source list))
  (labels ((from-list (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                      (handler-case (progn ;; maybe handler-case?
                                        (loop for item in source
                                           do (subscriber-next subs item))
                                        (subscriber-completed subs))
                        (error (cnd) (subscriber-error subs cnd)))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-list)))

;; Yeah, different words in loop so...
(defmethod observable-from ((source array))
  (labels ((from-list (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                      (handler-case (progn
                                        (loop for item across source
                                           do (subscriber-next subs item))
                                        (subscriber-completed subs))
                        (error (cnd) (subscriber-error subs cnd)))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-list)))

;; This would be a lot easier if the whole gray streams thingy had made it to
;; the standard. I'm not going to use trivial-gray-streams just for this.
;;
(defmacro read-stream (source eof-error-p &optional eof-value)
  "Read from a stream"
  `(if (subtypep 'character (stream-element-type ,source))
      (read-char ,source ,eof-error-p ,eof-value)
      (read-byte ,source ,eof-error-p ,eof-value)))

(defmethod observable-from ((source stream))
  (labels ((from-stream (subs) ;; this is the subscribe function
             (flet ((producer() ;; this will be called by the scheduler
                      (handler-case (progn
                                        (loop for byte = (read-stream source nil)
                                           while byte
                                           do (subscriber-next subs byte))
                                        (subscriber-completed subs))
                        (error (cnd) (subscriber-error subs cnd)))))
               (with-current-scheduler (scheduler)
                 (schedule scheduler #'producer)))))
    (make-observable #'from-stream)))
