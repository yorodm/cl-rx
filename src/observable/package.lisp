(in-package :cl-user)

(defpackage cl-rx.observable
  (:use :cl)
  (:export
   #:observable
   #:observable-just
   #:observable-from
   #:observable-subscribe
   #:observable-create
   #:observable-filter
   #:observable-take
   #:observable-find
   #:observable-all
   #:observable-map)
  (:import-from :trivial-gray-streams
               :fundamental-input-stream
               :fundamental-binary-input-stream
               :fundamental-character-input-stream)
  (:import-from :cl-rx.scheduler
                :schedule
                :schedule-relative
                :with-current-scheduler)
  (:import-from :cl-rx.subscriber
                :make-subscriber
                :subscriber
                :safe-subscriber
                :subscriber-next
                :subscriber-completed
                :subscriber-error))

(in-package :cl-rx.observable)
