(in-package :cl-rx.util)

(defmacro -> (what &body body)
  "Clojure style threading macro"
  (loop
     with x = what and form = (pop body)
     while (not (null form))
       when (listp form)
     do (setf x `(,(first form) ,x ,@(rest form))
              form (pop body))
       else
         do (setf x `(form x) form (pop body))
       end
     finally (return x)))
