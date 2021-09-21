(uiop:define-package :ploy/macroexpand
  (:use :ploy/prologue)
  (:shadow #:macroexpand #:macroexpand-1 #:eval)
  (:import-from :ploy/ir)
  (:export #:macroexpand-program))
(in-package :ploy/macroexpand)

(defgeneric macroexpand-walker (macros expr))

(defmethod macroexpand-walker ((macros list) (expr ir:let))
  (let* ((initform (macroexpand-walker macros (ir:initform expr)))
         (inner-macros (if (typep initform 'ir:macro)
                           (acons (ir:binding expr) initform macros)
                           macros))
         (body (macroexpand-walker inner-macros (ir:body expr))))
    (if (typep initform 'ir:macro)
        body
        (shallow-copy expr
                      :initform initform
                      :body body))))

(defgeneric resolve-to-macro (macros expr))

(defmethod resolve-to-macro ((macros list) (expr ir:expr))
  "Fallthrough: not a macro"
  (declare (ignorable macros expr))
  nil)

(defmethod resolve-to-macro ((macros list) (expr ir:macro))
  (declare (ignorable macros))
  expr)

(defmethod resolve-to-macro ((macros list) (expr ir:ident))
  (cdr (assoc expr macros :test #'ir:same-ident-p)))

(defgeneric eval (bindings expr))

(defgeneric truthyp (thing))

(defmethod truthyp ((thing ir:quote))
  (truthyp (ir:lit thing)))

(defmethod truthyp ((thing symbol))
  (ecase thing
    ((nil ploy-user:|false|) nil)
    ((t ploy-user:|true|) t)))

(defmethod eval ((bindings list) (expr ir:if))
  (with-slot-accessors (ir:predicate ir:then ir:else) expr
    (eval bindings
          (if (truthyp (eval bindings ir:predicate))
              ir:then
              ir:else))))

(defmethod eval ((bindings list) (expr ir:let))
  (with-slot-accessors (ir:initform ir:binding ir:body) expr
    (let* ((val (eval bindings ir:initform))
           (inner-scope (acons ir:binding val bindings)))
      (eval inner-scope ir:body))))

(defmethod eval ((bindings list) (expr ir:macro))
  (declare (ignorable bindings expr))
  (error "Cannot eval a macro!"))

(defmethod eval ((bindings list) (expr ir:ident))
  (if-let ((cell (assoc expr bindings :test #'ir:same-ident-p)))
    (cdr cell)
    (error "unbound identifier ~a" expr)))

(defmethod eval ((bindings list) (expr ir:prog2))
  (eval bindings (ir:discard expr))
  (eval bindings (ir:ret expr)))

(defmethod eval ((bindings list) (expr ir:comma))
  (declare (ignorable bindings))
  (error "Comma not inside a backquote: ~a" expr))

(define-class closure
    ((fn ir:fn)
     (bindings (list-of (cons ir:ident t)))))

(defmethod eval ((bindings list) (expr ir:fn))
  (declare (ignorable bindings))
  (make-instance 'closure
                 :fn expr
                 :bindings bindings))

(defmethod eval ((bindings list) (expr ir:quote))
  (declare (ignorable bindings))
  expr)

(defgeneric quasiquote (bindings expr))

(defmethod quasiquote (bindings (expr ir:ident))
  "Don't recurse inside idents, both to save work and to preserve eq"
  (declare (ignore bindings))
  expr)

(defmethod quasiquote (bindings (expr ir:expr))
  (ir:map-nested-exprs (curry #'quasiquote bindings) expr))

(defmethod quasiquote (bindings (expr ir:comma))
  (eval bindings (ir:term expr)))

(defmethod eval ((bindings list) (expr ir:call))
  (let* ((closure (eval bindings (ir:operator expr)))
         (args (mapcar (curry #'eval bindings) (ir:args expr))))
    (check-type closure closure)
    (let* ((fn (fn closure))
           (inner-scope (pairlis (ir:arglist fn) args (bindings closure))))
      (eval inner-scope (ir:body fn)))))

(defmethod eval ((bindings list) (expr ir:backquote))
  (quasiquote bindings (ir:term expr)))

(typedec #'macroexpand-1 (func (ir:macro (list-of ir:expr)) ir:expr))
(defun macroexpand-1 (macro args)
  (assert (= (length (the (list-of ir:ident) (ir:arglist macro)))
             (length args)))
  (eval (mapcar #'cons (ir:arglist macro) args)
        (ir:body macro)))

(typedec #'macroexpand
         (func ((list-of (cons ir:ident ir:macro)) ir:expr (list-of ir:expr))
               ir:expr))
(defun macroexpand (macros operator args)
  (if-let ((macro (resolve-to-macro macros operator)))
    (macroexpand-walker macros
                        (macroexpand-1 macro args))
    (make-instance 'ir:call
                   :operator operator
                   :args args)))

(defmethod macroexpand-walker ((macros list) (expr ir:call))
  (let* ((operator (macroexpand-walker macros (ir:operator expr)))
         (args (mapcar (curry #'macroexpand-walker macros) (ir:args expr))))
    (macroexpand macros operator args)))

(defmethod macroexpand-walker ((macros list) (expr ir:ident))
  "Don't recurse inside of idents, both to save work and to preserve eq"
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir:expr))
  (ir:map-nested-exprs (curry #'macroexpand-walker macros) expr))

(defmethod macroexpand-walker ((macros list) (expr ir:quote))
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir:backquote))
  (declare (ignorable macros))
  expr)

(defmethod macroexpand-walker ((macros list) (expr ir:comma))
  (error "comma not inside a backquote!"))

(typedec #'macroexpand-program
         (func (ir:expr &optional (list-of (cons ir:ident ir:macro))) ir:expr))
(defun macroexpand-program (program &optional macros)
  (macroexpand-walker macros program))
