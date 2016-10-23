(in-package cl-rx.observable)

(defgeneric observable-from (observable source)
  (:documentation "Creates a new observable from the given source"))
