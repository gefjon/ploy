(uiop:define-package ploy/ir1-expr
  (:use ploy/prologue)
  (:nicknames ir1)
  (:shadow prog2 let quote)
  (:export
   expr
   prog2 discard ret
   scope name contents parent body
   ident name scope
   let binding initform make-let
   fn arglist
   quote lit
   call operator args
   macro arglist

   find-ident make-ident

   *global-scope*))
(in-package ploy/ir1-expr)

(define-class expr
    ())

(define-class prog2
    ((discard expr)
     (ret expr))
  :superclasses (expr))

(define-class scope
    ((name unique-name)
     (contents (hash-map name ident)
               :initform (make-hash-table :test 'eq))
     (parent (or null scope))
     (body expr))
  :superclasses (expr))

(define-class ident
    ((name name)
     (scope scope))
  :superclasses (expr))

(defmethod print-object ((ident ident) stream)
  (print-unreadable-object (ident stream)
    (format stream "~a ~a " (class-name (class-of ident)) (name ident))
    (let* ((scope (scope ident)))
      (print-unreadable-object (scope stream)
        (format stream "~a ~a" (class-name (class-of scope)) (name scope))))))

(define-class let
    ((binding ident)
     (initform expr))
  :superclasses (scope))

(typedec #'make-let (func (scope symbol expr (func (scope) expr)) let))
(defun make-let (enclosing-scope name initform body-ctor)
  (let* ((new-scope (make-instance 'let
                                   :name (gensymify 'let name)
                                   :parent enclosing-scope
                                   :initform initform)))
    (setf (binding new-scope) (ir1:make-ident name new-scope)
          (body new-scope) (funcall body-ctor new-scope))
    new-scope))

(define-class fn
    ((arglist (list-of ident)))
  :superclasses (scope))

(define-class quote
    ((lit literal))
  :superclasses (expr))

(define-class call
    ((operator ident)
     (args (list-of ident)))
  :superclasses (expr))

(define-class macro
    ((arglist (list-of ident)))
  :superclasses (scope))

(typedec #'find-ident (func (name scope) ident))
(defun find-ident (name scope)
  (or (gethash name (contents scope))
      (and (parent scope) (find-ident name (parent scope)))
      (error "Unbound identifier ~a in scope ~a" name (name scope))))

(typedec #'make-ident (func (name scope) ident))
(defun make-ident (name scope)
  (setf (gethash name (contents scope))
        (make-instance 'ident
                       :name name
                       :scope scope)))

(defparameter *global-scope*
  (let* ((scope (make-instance 'scope :name 'global-scope :parent nil)))
    (ir1:make-ident 'ploy-user:+ scope)
    (ir1:make-ident 'ploy-user:|exit| scope)
    scope))
