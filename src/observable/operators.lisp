(in-package cl-rx.observable)

;; Let's keep all generics in one place please

;; Why use generics instead of just functions? It makes it easier for everyone
;; to extend the behavior of the library. Also it enforces a contract between the library and the client. See `conditionals.lisp' for an example

;; Creation
(defgeneric observable-from (source)
  (:documentation "Creates a new observable from the given source"))

(defgeneric observable-just (source)
  (:documentation "Creates a new observable that emits the source as it's only
  item"))

;; conditionals
(defgeneric observable-filter (observ predicate)
  (:documentation "Creates an observable sequence that will emit all the
  elements from OBSERV for which PREDICATE returns true"))

(defgeneric observable-take (observ n)
  (:documentation "Creates an observable sequence that will emit the first
  N elements in OBSERV"))

(defgeneric observable-first (observ)
  (:documentation "Returns the first element emitted by OBSERV"))

(defgeneric observable-all (observ predicate)
  (:documentation "Creates an observable sequence that contains a single element
  determining whether all elements in the source sequence satisfy the
  predicate"))

(defgeneric observable-map (observ fn)
  (:documentation "Applies FN to each item in OBSERV, creating a new observable
  sequence with the results"))

(defmacro protect-with((subs error-type) &body body)
  "A helper macro to write operators"
  `(handler-case
       (progn ,@body)
    (,error-type (cnd) (subscriber-error ,subs cnd))))
