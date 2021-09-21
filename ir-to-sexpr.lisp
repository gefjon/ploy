(uiop:define-package #:ploy/ir-to-sexpr
  (:import-from #:ploy/ir)
  (:use #:ploy/prologue)
  (:import-from #:ploy/type-infer
                #:apply-type-application)
  (:export #:output-program #:output-type #:output-expr))
(in-package #:ploy/ir-to-sexpr)

(defgeneric output-expr (expr))

(defun output-program (program)
  (output-expr program))

(defmethod output-expr ((expr ir:ident))
  (ir:name expr))

(defmethod output-expr ((expr ir:let))
  `(let ((,(ir:name (ir:binding expr))
           ,(output-expr (ir:initform expr))))
     ,@(when (ir:ignorablep expr)
        `((declare (ignorable ,(ir:name (ir:binding expr))))))
     ,(output-expr (ir:body expr))))

(defmethod output-expr ((expr ir:fn))
  `(lambda ,(mapcar #'ir:name (ir:arglist expr))
     ,(output-expr (ir:body expr))))

(defmethod output-expr ((expr ir:quote))
  (case (ir:lit expr)
    (ploy-user:|true| t)
    (ploy-user:|false| nil)
    (otherwise (ir:lit expr))))

(defmethod output-expr ((expr ir:if))
  (with-slot-accessors (ir:predicate ir:then ir:else) expr
    (list 'if
          (output-expr ir:predicate)
          (output-expr ir:then)
          (output-expr ir:else))))

(defmethod output-expr ((expr ir:call))
  `(funcall ,(output-expr (ir:operator expr))
            ,@(mapcar #'output-expr (ir:args expr))))

(defgeneric output-type (type))

(defmethod output-expr :around ((expr ir:expr))
  (let* ((body (call-next-method)))
    (if (and (ir:type-boundp expr)
             (typep (ir:type expr)
                    '(or ir:cl-type ir:fn-type)))
        (list 'the (output-type (ir:type expr)) body)
        body)))

(defmethod output-type ((type ir:cl-type))
  (ir:body type))

(defmethod output-type ((type ir:type-variable))
  (declare (ignorable type))
  t)

(defmethod output-type ((type ir:fn-type))
  `(function ,(mapcar #'output-type (ir:args type))
             (values ,(output-type (ir:ret type)) &optional)))

(defmethod output-type ((type ir:type-application))
  (output-type (apply-type-application type)))

(defmethod output-type ((type ir:forall-type))
  (output-type (ir:body type)))
