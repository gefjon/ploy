;;; transform s-expressions into CLOS `expr' objects; make explicit the construction, nesting and access of scopes.
(uiop:define-package ploy/ir1-passes/sexpr-to-ir1
  (:use ploy/prologue)
  (:import-from ploy/ir1-expr)
  (:import-from ploy/ploy-user)
  (:export parse-ir1))
(in-package ploy/ir1-passes/sexpr-to-ir1)

(defgeneric parse-ir1 (current-scope remaining-body form))

(defgeneric parse-ir1-form (current-scope remaining-body head &rest tail))

(defmethod parse-ir1 ((scope ir1:scope) (remaining-body list) (form cons))
  (apply #'parse-ir1-form scope remaining-body form))

(defmacro define-literals ()
  (cons 'cl:progn
        (iter (for literal in *literal-classes*)
          (collect `(defmethod parse-ir1 ((scope ir1:scope) (remaining-body null) (literal ,literal))
                      ,(format nil "Treat a ~a as a literal, returning it" literal)
                      (declare (ignorable scope remaining-body))
                      (make-instance 'ir1:quote :lit literal))))))
(define-literals)

(defmethod parse-ir1 ((scope ir1:scope) (remaining-body list) (form symbol))
  (assert (not remaining-body) ()
          "discarding read from variable ~a" form)
  (check-type form name)
  (ir1:find-ident form scope))

(typedec #'maybe-prog2 (func (ir1:scope ir1:expr list) ir1:expr))
(defun maybe-prog2 (scope first-expr remaining-body)
  (if remaining-body
      (make-instance 'ir1:prog2
                     :discard first-expr
                     :ret (parse-ir1 scope (rest remaining-body) (first remaining-body)))
      first-expr))

(defmethod parse-ir1-form ((scope ir1:scope) (remaining-body list) function &rest arglist)
  "Fallthrough: compile as a function call"
  (let* ((funcall (make-instance 'ir1:call
                                 :operator (parse-ir1 scope nil function)
                                 :args (mapcar (curry #'parse-ir1 scope nil) arglist))))
    (maybe-prog2 scope funcall remaining-body)))

(defmacro define-expr ((head &rest arglist) (scope remaining-body) &body body)
  `(defmethod parse-ir1-form
       ((,scope ir1:scope) (,remaining-body list) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(typedec #'scoped-lambda (func (ir1:scope symbol (list-of symbol) list &optional (or symbol class)) ir1:expr))
(defun scoped-lambda (enclosing-scope fn-name arglist body &optional (class 'ir1:fn))
  (let* ((scoped-fn (make-instance class
                                   :name (gensymify 'fn fn-name)
                                   :parent enclosing-scope)))
    (setf (ir1:arglist scoped-fn) (mapcar (rcurry #'ir1:make-ident scoped-fn) arglist)
          (ir1:body scoped-fn) (parse-ir1 scoped-fn (rest body) (first body)))
    scoped-fn))

(typedec #'let-binding-name-initform (func (ir1:scope (or symbol (list-of symbol)) list) (values symbol ir1:expr)))
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
    (ir1:make-let enclosing-scope name initform
                  (lambda (inner-scope) (parse-ir1 inner-scope (rest remaining-body) (first remaining-body))))))

(define-expr (ploy-user:|fn| arglist &body body) (enclosing-scope remaining-body)
  (assert (not remaining-body) ()
          "fn unused: ~a" `(ploy-user:|fn| ,arglist ,@body))
  (scoped-lambda enclosing-scope nil arglist body))

(define-expr (ploy-user:|scope| &body body) (enclosing-scope remaining-body)
  (let* ((inner-scope (make-instance 'ir1:scope
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
  (ir1:make-let enclosing-scope name (scoped-lambda enclosing-scope name arglist body 'ir1:macro)
                (lambda (inner-scope)
                  (parse-ir1 inner-scope (rest remaining-body) (first remaining-body)))))
