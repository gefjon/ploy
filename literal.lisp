(uiop:define-package #:ploy/literal
  (:use #:ploy/prologue)
  (:export #:*literal-classes* #:builtin-type-spec-for-literal #:literal))
(in-package #:ploy/literal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *literal-classes* `((fixnum . ploy-user:|fixnum|)
                                    ((eql 'ploy-user:|true|) . ploy-user:|boolean|)
                                    ((eql 'ploy-user:|false|) . ploy-user:|boolean|)
                                    ((eql 'ploy-user:|nil|) . (ploy-user:|forall| (|a|) (ploy-user:|list| |a|))))
    "An alist whose CARs are CLOS specializers and whose CDRs are the names of Ploy types."))

(defun eql-specializer-to-typespec (specializer)
  (destructuring-bind (eql (quote thing)) specializer
    (assert (eq eql 'eql))
    (assert (eq quote 'quote))
    (list 'eql thing)))

(defun specializer-typespec (specializer)
  (if (and (consp specializer)
           (eq (first specializer) 'eql))
      (eql-specializer-to-typespec specializer)
      specializer))

(defun specializerp (thing specializer)
  (typep thing (specializer-typespec specializer)))

(defun builtin-type-spec-for-literal (constant)
  (iter (for (specializer . type-name) in *literal-classes*)
    (when (specializerp constant specializer)
      (return type-name))
    (finally (error "cannot find type for constant ~a" constant))))

(deftype literal ()
  (cons 'or (mapcar (~> #'car #'specializer-typespec) *literal-classes*)))
