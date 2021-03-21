(uiop:define-package #:ploy/ir1-to-sexpr
  (:import-from #:ploy/ir1-expr)
  (:use #:ploy/prologue)
  (:import-from #:ploy/builtins #:*builtin-names*)
  (:export #:output-program))
(in-package #:ploy/ir1-to-sexpr)

(define-special *ident-symbols* (hash-map ir1:ident symbol))

(defgeneric output-expr (expr))

(defun builtin-ident-symbols ()
  (let* ((table (make-hash-table :test 'eq)))
    (iter (for ident in *builtin-names*)
      (setf (gethash ident table)
            (ir1:name ident)))
    table))

(defun output-program (program)
  (let* ((*ident-symbols* (builtin-ident-symbols)))
    (output-expr program)))

(defun ident-symbol (ident)
  (ensure-gethash ident *ident-symbols* (make-gensym (ir1:name ident))))

(defmethod output-expr ((expr ir1:ident))
  (ident-symbol expr))

(defmethod output-expr ((expr ir1:let))
  `(let ((,(output-expr (ir1:binding expr))
           ,(output-expr (ir1:initform expr))))
     ,(output-expr (ir1:body expr))))

(defmethod output-expr ((expr ir1:fn))
  `(lambda ,(mapcar #'output-expr (ir1:arglist expr))
     ,(output-expr (ir1:body expr))))

(defmethod output-expr ((expr ir1:quote))
  (ir1:lit expr))

(defmethod output-expr ((expr ir1:call))
  `(funcall ,(output-expr (ir1:operator expr))
            ,@(mapcar #'output-expr (ir1:args expr))))
