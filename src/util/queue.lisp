(in-package :cl-rx.util)
;; classes

(defclass queue ()
  ((list :initform nil)
   (tail :initform nil))
  (:documentation "A FIFO"))

(defclass priority-queue (queue)
  ()
  (:documentation "A priority queue"))

;; generics

(defgeneric enqueue (item queue)
  (:documentation "Adds a new item to the TAIL of the queue"))

(defgeneric dequeue (queue)
  (:documentation "Takes an item from the queue"))

(defgeneric top (queue)
  (:documentation "Returns the first item from the queue without removing it"))

(defgeneric empty-p (queue)
  (:documentation "Returns true if the queue is empty"))

;; utility functions
(defmethod empty-p ((queue queue))
  "Checks if a queue is empty"
  (with-slots (list) queue
    (null list)))

;; public interface
(defmethod enqueue (new-item (queue queue))
  ;; Tail wagging makes things easier
    (with-slots (list tail) queue
      (let ((new-tail (list new-item)))
        (cond ((null list) (setf list new-tail))
              (t (setf (cdr tail) new-tail)))
        (setf tail new-tail)))
    queue)

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))
