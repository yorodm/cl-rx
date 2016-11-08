(in-package :cl-rx-test)

;; Testing the OBSERVER-FROM operators in different sources
;; NOTE: To run this test file, execute `(asdf:test-system :cl-rx)' in your Lisp.


;; blah blah blah.
(plan 4)
(subtest "Testing observable-from with a list"
  (let ((observ (observable-from *source-list*))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* (reverse (list 1 2 3 4 5 6 7 8 9)))))

(subtest "Testing observable-from with an array"
  (let ((observ (observable-from *source-array*))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x) (push x *special-var*)))
    (is *special-var* (reverse (list 1 2 3 4 5)))))

(subtest "Testing observable-from with a stream"
  (let ((observ (observable-from *source-stream*))
        (count-var 0))
    (observable-subscribe observ (lambda (x)
                                   (declare (ignore x))
                                   (incf count-var)))
    (is count-var (length "This is the data"))))

(subtest "Testing observable-just"
  (let ((observ (observable-just *source-list*))
        (*special-var* nil))
    (observable-subscribe observ (lambda (x)
                                   (setf *special-var* x)))
    (is *special-var* *source-list*)))
(finalize)
