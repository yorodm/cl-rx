(in-package :cl-rx-test)

(subtest "Testing the priority queue"
    (let ((queue (make-instance 'pqueue)))
      (subtest "Testing enqueue"
        (diag "Adding element with priority 15")
        (enqueue queue 15 "Data for 15")
        (diag "Adding element with priority 115")
        (enqueue queue 115 "Data for 115")
        (diag "Adding element with priority 14")
        (enqueue queue 14 "Data for 14")
        (diag "Adding element with priority 14 (duplicate)")
        (enqueue queue 14 "Data for 14")
        (diag "Adding element with priority 10")
        (enqueue queue 10 "Data for 10")
        (diag "Checking the first element in the queue")
        (is (top queue) '(10 . "Data for 10")))
      (subtest "Testing dequeue"
        (is (dequeue queue) '(10 . "Data for 10"))
        (is (dequeue queue) '(14 . "Data for 14"))
        (is (dequeue queue) '(14 . "Data for 14"))
        (is (dequeue queue) '(15 . "Data for 15"))
        (is (dequeue queue) '(115 . "Data for 115")))))
