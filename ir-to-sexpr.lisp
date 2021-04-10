(uiop:define-package #:ploy/ir-to-sexpr
  (:import-from #:ploy/ir)
  (:use #:ploy/prologue)
  (:export #:output-program))
(in-package #:ploy/ir-to-sexpr)

(defgeneric output-expr (expr))

(defun output-program (program)
  (output-expr program))

(defmethod output-expr ((expr ir:ident))
  (ir:name expr))

(defmethod output-expr ((expr ir:let))
  `(let ((,(output-expr (ir:binding expr))
           ,(output-expr (ir:initform expr))))
     ,(output-expr (ir:body expr))))

(defmethod output-expr ((expr ir:fn))
  `(lambda ,(mapcar #'output-expr (ir:arglist expr))
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
