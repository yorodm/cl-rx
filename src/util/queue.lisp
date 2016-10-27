(in-package :cl-rx.util-internal)

(defclass pqueue ()
  ((elements :initform (make-array 15 :fill-pointer 0 :adjustable t)
             :accessor elements))
  (:documentation "A vector based priority queue. Elements must be in the form
(PRIORITY . ITEM)"))

(defmacro // (a b)
  "Python-like integer division"
  `(truncate ,a ,b))

(defun elt-priority (heap i)
  "Returns the priority of the element at position I"
  (item-priority (elt (elements heap) i)))

(defun elt-data (heap i)
  "Returns the DATA of the element at position I"
  (item-data (elt (elements heap) i)))

(defun item-priority (element)
  (car element))

(defun item-data (element)
  (cdr element))

(defun size (heap)
  (length (elements heap)))

(defun parent (i)
  "Parent of node I"
  (declare (optimize debug))
  (// (1- i) 2))

(defun left (i)
  "Left element of I"
  (+ (* 2 i) 1))

(defun right (i)
  "Right element of I"
  (+ (* 2 i) 2))

(defun has-left (heap i)
  "Checks if I has a left element"
  (< (left i) (size heap)))

(defun has-right (heap i)
  "Checks if I has a left element"
  (< (right i) (size heap)))

(defun swap (heap i j)
  "Swaps elements at indexes I and J"
  (rotatef (elt (elements heap) i)
           (elt (elements heap) j)))

(defun up-heap(heap i)
  "Fixes priorities after insertion of element at I"
  (let ((par (parent i)))
    (when (and (> i 0 )
               (< (elt-priority heap i)
                  (elt-priority heap par)))
      (swap heap i par)
      (up-heap heap par))))

(defun down-heap (heap i)
  "Fixes priorities after removal of element at I"
  (let ((lft (when (has-left heap i) (left i)))
        (rght (when (has-right heap i) (right i)))
        smaller)
  (when lft
    (setf smaller lft)
    (when (and rght
      (< (elt-priority heap rght) (elt-priority heap lft)))
           (setf smaller rght))
    (when (< (elt-priority heap smaller) (elt-priority heap i))
      (swap heap i smaller)
      (down-heap heap smaller)))))

(defun enqueue (heap key data)
  (vector-push-extend (cons key data) (elements heap) 15)
  (up-heap heap (1- (size heap))))

(defun dequeue (heap)
  "Removes and returns the first element in the queue"
  (when (> (size heap) 0)
    (swap heap 0 (1- (size heap)))
    (let ((item (vector-pop (elements heap))))
      (down-heap heap 0)
      item)))

(defun top (heap)
  "Returns the data of the first element in the queue"
  (when (> (size heap) 0)
  (item-data heap 0)))
