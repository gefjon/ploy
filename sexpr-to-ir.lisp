;;; transform s-expressions into CLOS `expr' objects; make explicit the construction, nesting and access of scopes.
(uiop:define-package :ploy/sexpr-to-ir
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:import-from #:ploy/ploy-user)
  (:export
   #:*empty-parser-scope*

   #:find-type #:find-ident

   #:parse-program #:parse-expr #:parse-type

   #:parser-scope #:name #:types #:terms #:parent))
(in-package :ploy/sexpr-to-ir)

(define-class parser-scope
    ((name symbol)
     (types (hash-map symbol ir:type-variable)
            :initform (make-hash-table :test 'eq))
     (terms (hash-map symbol ir:ident)
            :initform (make-hash-table :test 'eq))
     (parent (or null parser-scope))))

(typedec *empty-parser-scope* parser-scope)
(defparameter *empty-parser-scope* (make-instance 'parser-scope
                                                  :name 'empty-scope
                                                  :parent nil))

(typedec #'make-ident (func (symbol parser-scope) ir:ident))
(defun make-ident (name scope)
  (setf (gethash name (terms scope))
        (ir:gen-ident name)))

(typedec #'find-ident (func (symbol parser-scope) ir:ident))
(defun find-ident (name scope)
  (or (gethash name (terms scope))
      (and (parent scope) (find-ident name (parent scope)))
      (make-ident name scope)))

(typedec #'add-type (func (symbol ir:type parser-scope) ir:type))
(defun add-type (name type scope)
  (setf (gethash name (types scope))
        type))

(typedec #'make-type (func (symbol parser-scope &optional (or symbol class)) ir:type))
(defun make-type (name scope &optional (class 'ir:type-variable) )
  (assert (subtypep* class 'ir:type))
  (add-type name
            (make-instance class
                           :name name)
            scope))

(typedec #'find-type (func (symbol parser-scope) ir:type-variable))
(defun find-type (name scope)
  (or (gethash name (types scope))
      (and (parent scope) (find-type name (parent scope)))
      
      (make-type name scope)))

(typedec #'make-let (func (parser-scope symbol ir:expr (func (parser-scope) ir:expr)) ir:let))
(defun make-let (enclosing-scope name initform body-ctor)
  (let* ((new-scope (make-instance 'parser-scope
                                   :parent enclosing-scope
                                   :name (gensymify 'let name)))
         (binding (make-ident name new-scope)))
    (make-instance 'ir:let
                   :binding binding
                   :initform initform
                   :body (funcall body-ctor new-scope))))

(defgeneric parse-expr (current-scope remaining-body form))

(macrolet ((define-parse-identity (class)
             `(defmethod parse-expr ((scope parser-scope) (remaining-body list) (form ,class))
                (maybe-prog2 scope form remaining-body))))
  (define-parse-identity ir:expr)
  (define-parse-identity ir:type))

(defgeneric parse-expr-form (current-scope remaining-body head &rest tail))

(defmethod parse-expr ((scope parser-scope) (remaining-body list) (form cons))
  (apply #'parse-expr-form scope remaining-body form))

(defgeneric parse-type (current-scope form))

(defgeneric parse-type-form (current-scope head &rest tail))

(defmethod parse-type ((scope parser-scope) (form ir:type))
  (declare (ignorable scope))
  form)

(defmethod parse-type ((scope parser-scope) (form list))
  (apply #'parse-type-form scope form))

(defmethod parse-type ((scope parser-scope) (form symbol))
  (find-type form scope))

(defmacro define-type ((head &rest arglist) (scope) &body body)
  `(defmethod parse-type-form ((,scope parser-scope) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(defmethod parse-type-form ((scope parser-scope) constructor &rest args)
  "Fallback: parse as an `ir:type-application'"
  (make-instance 'ir:type-application
                 :constructor (parse-type scope constructor)
                 :args (mapcar (curry #'parse-type scope) args)))

(define-type (ploy-user:|fn| args ret) (scope)
  (make-instance 'ir:fn-type
                 :args (mapcar (curry #'parse-type scope) args)
                 :ret (parse-type scope ret)))


(define-type (ploy-user:|forall| args body) (scope)
  (let* ((inner-scope (make-instance 'parser-scope
                                     :parent scope))
         (args (iter (for name in args)
                 (collect (make-type name inner-scope)))))
    (make-instance 'ir:forall-type
                   :args args
                   :body (parse-type inner-scope body))))

(define-type (ploy-user:|cl-type| body) (scope)
  (make-instance 'ir:cl-type
                 :body body))

(defmacro define-literals (&rest classes)
  (cons 'cl:progn
        (iter (for specializer in classes)
          (collect `(defmethod parse-expr ((scope parser-scope) (remaining-body null) (literal ,specializer))
                      ,(format nil "Treat a ~a as a literal, returning it" specializer)
                      (declare (ignorable scope))
                      (assert (null remaining-body))
                      (make-instance 'ir:quote :lit literal))))))
(define-literals fixnum (eql 'ploy-user:|false|) (eql 'ploy-user:|true|))

(defmethod parse-expr ((scope parser-scope) (remaining-body list) (form symbol))
  (assert (not remaining-body) ()
          "discarding read from variable ~a" form)
  (check-type form symbol)
  (find-ident form scope))

(typedec #'maybe-prog2 (func (parser-scope ir:expr list) ir:expr))
(defun maybe-prog2 (scope first-expr remaining-body)
  (if remaining-body
      (make-instance 'ir:prog2
                     :discard first-expr
                     :ret (parse-expr scope (rest remaining-body) (first remaining-body)))
      first-expr))

(defmethod parse-expr-form ((scope parser-scope) (remaining-body list) function &rest arglist)
  "Fallthrough: compile as a function call"
  (let* ((funcall (make-instance 'ir:call
                                 :operator (parse-expr scope nil function)
                                 :args (mapcar (curry #'parse-expr scope nil) arglist))))
    (maybe-prog2 scope funcall remaining-body)))

(defmacro define-expr ((head &rest arglist) (scope remaining-body) &body body)
  `(defmethod parse-expr-form
       ((,scope parser-scope) (,remaining-body list) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(typedec #'scoped-lambda (func (parser-scope symbol (list-of symbol) list &optional symbol) ir:expr))
(defun scoped-lambda (enclosing-scope fn-name arglist body &optional (class 'ir:fn))
  (let* ((inner-scope (make-instance 'parser-scope
                                     :name (gensymify class fn-name)
                                     :parent enclosing-scope)))
    (make-instance class
                   :arglist (mapcar (rcurry #'make-ident inner-scope) arglist)
                   :body (parse-expr inner-scope (rest body) (first body)))))

(typedec #'let-binding-name-initform (func (parser-scope (or symbol (list-of symbol)) list) (values symbol ir:expr)))
(defun let-binding-name-initform (enclosing-scope name-or-function value-or-body)
  (etypecase name-or-function
    (list (let* ((name (first name-or-function))
                 (arglist (rest name-or-function)))
            (values name (scoped-lambda enclosing-scope name arglist value-or-body))))
    (symbol (assert (= (length value-or-body) 1) ()
                    "Malformed non-function let ~a should have exactly one value form but found ~a"
                    name-or-function value-or-body)
     (values name-or-function (parse-expr enclosing-scope nil (first value-or-body))))))

(define-expr (ploy-user:|let| name-or-function &body value-or-body) (enclosing-scope remaining-body)
  (assert remaining-body ()
          "let-binding unused: ~a" name-or-function)
  (multiple-value-bind (name initform)
      (let-binding-name-initform enclosing-scope name-or-function value-or-body)
    (make-let enclosing-scope name initform
                  (lambda (inner-scope) (parse-expr inner-scope (rest remaining-body) (first remaining-body))))))

(define-expr (ploy-user:|fn| arglist &body body) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "fn unused: ~a" `(ploy-user:|fn| ,arglist ,@body))
  (scoped-lambda enclosing-scope nil arglist body))

(define-expr (ploy-user:|scope| &body body) (enclosing-scope remaining-body)
  (let* ((inner-scope (make-instance 'parser-scope
                                     :name (gensymify 'scope)
                                     :parent enclosing-scope)))
    (setf (ir:body inner-scope)
          (parse-expr inner-scope (rest body) (first body)))
    (maybe-prog2 enclosing-scope
                 (apply #'maybe-prog2 inner-scope body)
                 remaining-body)))

(define-expr (ploy-user:|macro| arglist &body body) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "macro unused: ~a" `(ploy-user:|macro| ,arglist ,@body))
  (scoped-lambda enclosing-scope nil arglist body 'ir:macro))

(define-expr (ploy-user:|macrolet| (name &rest arglist) &body body) (enclosing-scope remaining-body)
  (make-let enclosing-scope name (scoped-lambda enclosing-scope name arglist body 'ir:macro)
                (lambda (inner-scope)
                  (parse-expr inner-scope (rest remaining-body) (first remaining-body)))))

(define-expr (ploy-user:|backquote| term) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "backquote result unused: ~a" `(ploy-user:|backquote| ,term))
  (make-instance 'ir:backquote
                 :term (parse-expr enclosing-scope nil term)))

(define-expr (ploy-user:|comma| term) (enclosing-scope remaining-body)
  (make-instance 'ir:comma
                 :term (parse-expr enclosing-scope remaining-body term)))

(define-expr (ploy-user:|quote| term) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "quoted literal unused: ~a" term)
  (let* ((quoted (parse-expr enclosing-scope nil term)))
    ;; avoid double-quoting literals, since their `parse-expr' method constructs a `quote' object
    (if (typep quoted 'ir:quote)
        quoted
        (make-instance 'ir:quote :lit quoted))))

(define-expr (ploy-user:|if| predicate then &optional (else 'ploy-user:|nil|))
    (scope remaining-body)
  (maybe-prog2 scope
               (make-instance 'ir:if
                              :predicate (parse-expr scope nil predicate)
                              :then (parse-expr scope nil then)
                              :else (parse-expr scope nil else))
               remaining-body))

(define-expr (ploy-user:|the| type term) (scope remaining-body)
  (maybe-prog2 scope (make-instance 'ir:the
                                    :type (parse-type scope type)
                                    :term (parse-expr scope nil term))
               remaining-body))

(typedec #'parse-program (func (list parser-scope) ir:expr))
(defun parse-program (program scope)
  (parse-expr scope (rest program) (first program)))
