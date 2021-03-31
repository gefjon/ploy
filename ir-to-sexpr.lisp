(uiop:define-package #:ploy/ir-to-sexpr
  (:import-from #:ploy/ir)
  (:use #:ploy/prologue)
  (:import-from #:ploy/builtins #:*builtin-names*)
  (:export #:output-program))
(in-package #:ploy/ir-to-sexpr)

(define-special *ident-symbols* (hash-map ir:ident symbol))

(defgeneric output-expr (expr))

(defun builtin-ident-symbols ()
  (let* ((table (make-hash-table :test 'eq)))
    (iter (for ident in *builtin-names*)
      (setf (gethash ident table)
            (ir:name ident)))
    table))

(defun output-program (program)
  (let* ((*ident-symbols* (builtin-ident-symbols)))
    (output-expr program)))

(defun ident-symbol (ident)
  (ensure-gethash ident *ident-symbols* (make-gensym (ir:name ident))))

(defmethod output-expr ((expr ir:ident))
  (ident-symbol expr))

(defmethod output-expr ((expr ir:let))
  `(let ((,(output-expr (ir:binding expr))
           ,(output-expr (ir:initform expr))))
     ,(output-expr (ir:body expr))))

(defmethod output-expr ((expr ir:fn))
  `(lambda ,(mapcar #'output-expr (ir:arglist expr))
     ,(output-expr (ir:body expr))))

(defmethod output-expr ((expr ir:quote))
  (ir:lit expr))

(defmethod output-expr ((expr ir:call))
  `(funcall ,(output-expr (ir:operator expr))
            ,@(mapcar #'output-expr (ir:args expr))))
