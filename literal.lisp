(uiop:define-package #:ploy/literal
  (:use #:ploy/prologue)
  (:import-from #:ploy/builtins #:find-builtin-type)
  (:export #:*literal-classes* #:builtin-type-for-literal #:literal))
(in-package #:ploy/literal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *literal-classes* '((fixnum . ploy-user:|fixnum|)
                                    ((eql 'ploy-user:|true|) . ploy-user:|boolean|)
                                    ((eql 'ploy-user:|false|) . ploy-user:|boolean|))
    "An alist whose CARs are CLOS specializers and whose CDRs are the names of Ploy types."))

(defun builtin-type-for-literal (constant)
  (find-builtin-type
   (iter (for (specializer . type-name) in *literal-classes*)
     (when (typep constant specializer)
       (return type-name))
     (finally (error "cannot find type for constant ~a" constant)))))

(deftype literal ()
  (cons 'or (mapcar #'car *literal-classes*)))
