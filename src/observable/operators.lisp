(in-package cl-rx.observable)

;; Here is the list of all supported operators

(defgeneric observable-from (source)
  (:documentation "Creates a new observable from the given source"))

(defgeneric observable-filter (observ predicate)
  (:documentation "Creates an observable sequence that will emit all the
  elements from OBSERV for which PREDICATE returns true"))

(defgeneric observable-take (observ n)
  (:documentation "Creates an observable sequence that will emit the first
  N elements in OBSERV"))
