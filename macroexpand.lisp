(uiop:define-package ploy/macroexpand
  (:use ploy/prologue)
  (:shadow macroexpand macroexpand-1 eval)
  (:import-from ploy/ir1-expr)
  (:export macroexpand-program))
(in-package ploy/macroexpand)

(defgeneric macroexpand-walker (macros expr))

(defmethod macroexpand-walker ((macros list) (expr ir1:let))
  (let* ((initform (macroexpand-walker macros (ir1:initform expr)))
         (inner-macros (if (typep initform 'ir1:macro)
                           (acons (ir1:binding expr) initform macros)
                           macros))
         (body (macroexpand-walker inner-macros (ir1:body expr))))
    (if (typep initform 'ir1:macro)
        body
        (shallow-copy expr
                      :initform initform
                      :body body))))

(defgeneric resolve-to-macro (macros expr))

(defmethod resolve-to-macro ((macros list) (expr ir1:expr))
  "Fallthrough: not a macro"
  (declare (ignorable macros expr))
  nil)

(defmethod resolve-to-macro ((macros list) (expr ir1:macro))
  (declare (ignorable macros))
  expr)

(defmethod resolve-to-macro ((macros list) (expr ir1:ident))
  (cdr (assoc expr macros)))

(defgeneric eval (bindings expr))

(defmethod eval ((bindings list) (expr ir1:let))
  (with-slot-accessors (ir1:initform ir1:binding ir1:body) expr
    (let* ((val (eval bindings ir1:initform))
           (inner-scope (acons ir1:binding val bindings)))
      (eval inner-scope ir1:body))))

(defmethod eval ((bindings list) (expr ir1:macro))
  (declare (ignorable bindings expr))
  (error "Cannot eval a macro!"))

(defmethod eval ((bindings list) (expr ir1:ident))
  (if-let ((cell (assoc expr bindings)))
    (cdr cell)
    (error "unbound identifier ~a" expr)))

(defmethod eval ((bindings list) (expr ir1:prog2))
  (eval bindings (ir1:discard expr))
  (eval bindings (ir1:ret expr)))

(defmethod eval ((bindings list) (expr ir1:comma))
  (declare (ignorable bindings))
  (error "Comma not inside a backquote: ~a" expr))

(define-class closure
    ((fn ir1:fn)
     (bindings (list-of (cons ir1:ident t)))))

(defmethod eval ((bindings list) (expr ir1:fn))
  (declare (ignorable bindings))
  (make-instance 'closure
                 :fn expr
                 :bindings bindings))

(defmethod eval ((bindings list) (expr ir1:quote))
  (declare (ignorable bindings))
  expr)

(defgeneric quasiquote (bindings expr))

(defmethod quasiquote (bindings (expr ir1:ident))
  "Don't recurse inside idents, both to save work and to preserve eq"
  (declare (ignore bindings))
  expr)

(defmethod quasiquote (bindings (expr ir1:expr))
  (ir1:map-nested-exprs (curry #'quasiquote bindings) expr))

(defmethod quasiquote (bindings (expr ir1:comma))
  (eval bindings (ir1:term expr)))

(defmethod eval ((bindings list) (expr ir1:call))
  (let* ((closure (eval bindings (ir1:operator expr)))
         (args (mapcar (curry #'eval bindings) (ir1:args expr))))
    (check-type closure closure)
    (let* ((fn (fn closure))
           (inner-scope (pairlis (ir1:arglist fn) args (bindings closure))))
      (eval inner-scope (ir1:body fn)))))

(defmethod eval ((bindings list) (expr ir1:backquote))
  (quasiquote bindings (ir1:term expr)))

(typedec #'macroexpand-1 (func (ir1:macro (list-of ir1:expr)) ir1:expr))
(defun macroexpand-1 (macro args)
  (assert (= (length (ir1:arglist macro)) (length args)))
  (eval (mapcar #'cons (ir1:arglist macro) args)
        (ir1:body macro)))

(typedec #'macroexpand
         (func ((list-of (cons ir1:ident ir1:macro)) ir1:expr (list-of ir1:expr))
               ir1:expr))
(defun macroexpand (macros operator args)
  (if-let ((macro (resolve-to-macro macros operator)))
    (macroexpand-walker macros
                        (macroexpand-1 macro args))
    (make-instance 'ir1:call
                   :operator operator
                   :args args)))

(defmethod macroexpand-walker ((macros list) (expr ir1:call))
  (let* ((operator (macroexpand-walker macros (ir1:operator expr)))
         (args (mapcar (curry #'macroexpand-walker macros) (ir1:args expr))))
    (macroexpand macros operator args)))

(defmethod macroexpand-walker ((macros list) (expr ir1:ident))
  "Don't recurse inside of idents, both to save work and to preserve eq"
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir1:expr))
  (ir1:map-nested-exprs (curry #'macroexpand-walker macros) expr))

(defmethod macroexpand-walker ((macros list) (expr ir1:quote))
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir1:backquote))
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir1:comma))
  (error "comma not inside a backquote!"))

(typedec #'macroexpand-program
         (func (ir1:expr &optional (list-of (cons ir1:ident ir1:macro))) ir1:expr))
(defun macroexpand-program (program &optional macros)
  (macroexpand-walker macros program))
