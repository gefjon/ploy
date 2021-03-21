;;; transform s-expressions into CLOS `expr' objects; make explicit the construction, nesting and access of scopes.
(uiop:define-package #:ploy/sexpr-to-ir1
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir1-expr)
  (:import-from #:ploy/ploy-user)
  (:import-from #:ploy/builtins #:*builtin-names*)
  (:export
   #:parse-program

   #:scope #:name #:contents #:parent))
(in-package #:ploy/sexpr-to-ir1)

(define-class scope
    ((name (or unique-name (eql :global-scope)))
     (contents (hash-map name ident)
               :initform (make-hash-table :test 'eq))
     (parent (or null scope))))

(typedec #'make-ident (func (name scope) ir1:ident))
(defun make-ident (name scope)
  (setf (gethash name (contents scope))
        (ir1:gen-ident name)))

(typedec #'find-ident (func (name scope) ir1:ident))
(defun find-ident (name scope)
  (or (gethash name (contents scope))
      (and (parent scope) (find-ident name (parent scope)))
      (make-ident name scope)))

(defun global-scope ()
  (let* ((scope (make-instance 'scope :name :global-scope :parent nil)))
    (iter (for binding in *builtin-names*)
      (setf (gethash (ir1:name binding) (contents scope)) binding))
    scope))

(typedec #'make-let (func (scope symbol ir1:expr (func (scope) ir1:expr)) ir1:let))
(defun make-let (enclosing-scope name initform body-ctor)
  (let* ((new-scope (make-instance 'scope
                                   :parent enclosing-scope
                                   :name (gensymify 'let name)))
         (binding (make-ident name new-scope)))
    (make-instance 'ir1:let
                   :binding binding
                   :initform initform
                   :body (funcall body-ctor new-scope))))

(defgeneric parse-ir1 (current-scope remaining-body form))

(defgeneric parse-ir1-form (current-scope remaining-body head &rest tail))

(defmethod parse-ir1 ((scope scope) (remaining-body list) (form cons))
  (apply #'parse-ir1-form scope remaining-body form))

(defmacro define-literals ()
  (cons 'cl:progn
        (iter (for literal in *literal-classes*)
          (collect `(defmethod parse-ir1 ((scope scope) (remaining-body null) (literal ,literal))
                      ,(format nil "Treat a ~a as a literal, returning it" literal)
                      (declare (ignorable scope remaining-body))
                      (make-instance 'ir1:quote :lit literal))))))
(define-literals)

(defmethod parse-ir1 ((scope scope) (remaining-body list) (form symbol))
  (assert (not remaining-body) ()
          "discarding read from variable ~a" form)
  (check-type form name)
  (find-ident form scope))

(typedec #'maybe-prog2 (func (scope ir1:expr list) ir1:expr))
(defun maybe-prog2 (scope first-expr remaining-body)
  (if remaining-body
      (make-instance 'ir1:prog2
                     :discard first-expr
                     :ret (parse-ir1 scope (rest remaining-body) (first remaining-body)))
      first-expr))

(defmethod parse-ir1-form ((scope scope) (remaining-body list) function &rest arglist)
  "Fallthrough: compile as a function call"
  (let* ((funcall (make-instance 'ir1:call
                                 :operator (parse-ir1 scope nil function)
                                 :args (mapcar (curry #'parse-ir1 scope nil) arglist))))
    (maybe-prog2 scope funcall remaining-body)))

(defmacro define-expr ((head &rest arglist) (scope remaining-body) &body body)
  `(defmethod parse-ir1-form
       ((,scope scope) (,remaining-body list) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(typedec #'scoped-lambda (func (scope symbol (list-of symbol) list &optional symbol) ir1:expr))
(defun scoped-lambda (enclosing-scope fn-name arglist body &optional (class 'ir1:fn))
  (let* ((inner-scope (make-instance 'scope
                                     :name (gensymify class fn-name)
                                     :parent enclosing-scope)))
    (make-instance class
                   :arglist (mapcar (rcurry #'make-ident inner-scope) arglist)
                   :body (parse-ir1 inner-scope (rest body) (first body)))))

(typedec #'let-binding-name-initform (func (scope (or symbol (list-of symbol)) list) (values symbol ir1:expr)))
(defun let-binding-name-initform (enclosing-scope name-or-function value-or-body)
  (etypecase name-or-function
    (list (let* ((name (first name-or-function))
                 (arglist (rest name-or-function)))
            (values name (scoped-lambda enclosing-scope name arglist value-or-body))))
    (symbol (assert (= (length value-or-body) 1) ()
                    "Malformed non-function let ~a should have exactly one value form but found ~a"
                    name-or-function value-or-body)
     (values name-or-function (parse-ir1 enclosing-scope nil (first value-or-body))))))

(define-expr (ploy-user:|let| name-or-function &body value-or-body) (enclosing-scope remaining-body)
  (assert remaining-body ()
          "let-binding unused: ~a" name-or-function)
  (multiple-value-bind (name initform)
      (let-binding-name-initform enclosing-scope name-or-function value-or-body)
    (make-let enclosing-scope name initform
                  (lambda (inner-scope) (parse-ir1 inner-scope (rest remaining-body) (first remaining-body))))))

(define-expr (ploy-user:|fn| arglist &body body) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "fn unused: ~a" `(ploy-user:|fn| ,arglist ,@body))
  (scoped-lambda enclosing-scope nil arglist body))

(define-expr (ploy-user:|scope| &body body) (enclosing-scope remaining-body)
  (let* ((inner-scope (make-instance 'scope
                                     :name (gensymify 'scope)
                                     :parent enclosing-scope)))
    (setf (ir1:body inner-scope)
          (parse-ir1 inner-scope (rest body) (first body)))
    (maybe-prog2 enclosing-scope inner-scope remaining-body)))

(define-expr (ploy-user:|macro| arglist &body body) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "macro unused: ~a" `(ploy-user:|macro| ,arglist ,@body))
  (scoped-lambda enclosing-scope nil arglist body 'ir1:macro))

(define-expr (ploy-user:|macrolet| (name &rest arglist) &body body) (enclosing-scope remaining-body)
  (make-let enclosing-scope name (scoped-lambda enclosing-scope name arglist body 'ir1:macro)
                (lambda (inner-scope)
                  (parse-ir1 inner-scope (rest remaining-body) (first remaining-body)))))

(define-expr (ploy-user:|backquote| term) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "backquote result unused: ~a" `(ploy-user:|backquote| ,term))
  (make-instance 'ir1:backquote
                 :term (parse-ir1 enclosing-scope nil term)))

(define-expr (ploy-user:|comma| term) (enclosing-scope remaining-body)
  (make-instance 'ir1:comma
                 :term (parse-ir1 enclosing-scope remaining-body term)))

(define-expr (ploy-user:|quote| term) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "quoted literal unused: ~a" term)
  (if (typep term 'literal)
      ;; avoid double-quoting literals
      (parse-ir1 enclosing-scope nil term)
      (make-instance 'quote
                     :term (parse-ir1 enclosing-scope nil term))))

(typedec #'parse-program (func (list &optional scope) ir1:expr))
(defun parse-program (program &optional (scope (global-scope)))
  (parse-ir1 scope (rest program) (first program)))
