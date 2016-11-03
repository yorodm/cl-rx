(in-package cl-rx.observable)

(defgeneric observable-from (source)
  (:documentation "Creates a new observable from the given source"))
