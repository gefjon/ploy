(uiop:define-package #:ploy/type-infer
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:import-from #:ploy/builtins
                #:find-builtin-type)
  (:import-from #:ploy/literal
                #:builtin-type-for-literal))
(in-package #:ploy/type-infer)

(declaim (optimize (speed 0) (debug 3)))

(define-class constraint ())

(define-class eq-constraint
    ((lhs ir:type)
     (rhs ir:type))
  :superclasses (constraint))

(defun must-be-eq (lhs rhs)
  (make-instance 'eq-constraint
                 :lhs lhs
                 :rhs rhs))

(defgeneric annotate-types (expr))

(defun add-type-variable (expr &optional (name (make-gensym 'type-variable-)))
  (if (ir:type-boundp expr) expr
      (shallow-copy expr
                    :type (make-instance 'ir:type-variable
                                         :name name))))

(defmethod annotate-types ((expr ir:ident))
  (add-type-variable expr (ir:name expr)))

(defmethod annotate-types ((expr ir:expr))
  (ir:map-nested-exprs #'annotate-types (add-type-variable expr)))

(defgeneric collect-constraints (expr)
  (:method-combination append))

(defmethod collect-constraints append ((expr ir:expr) &aux constraints)
  (flet ((visit (subexpr)
           (setf constraints (append (collect-constraints subexpr) constraints))
           subexpr))
    (ir:map-nested-exprs #'visit expr))
  constraints)

(defmethod collect-constraints append ((expr ir:call))
  (list (must-be-eq (ir:type (ir:operator expr))
                    (make-instance 'ir:fn-type
                                   :args (mapcar #'ir:type (ir:args expr))
                                   :ret (ir:type expr)))))

(defmethod collect-constraints append ((expr ir:if))
  (list (must-be-eq (ir:type (ir:predicate expr))
                    (find-builtin-type 'ploy-user:|boolean|))
        (must-be-eq (ir:type (ir:then expr))
                    (ir:type (ir:else expr)))
        (must-be-eq (ir:type expr)
                    (ir:type (ir:then expr)))))

(defmethod collect-constraints append ((expr ir:let))
  (list (must-be-eq (ir:type (ir:binding expr))
                    (ir:type (ir:initform expr)))
        (must-be-eq (ir:type expr)
                    (ir:type (ir:body expr)))))

(defmethod collect-constraints append ((expr ir:prog2))
  (list (must-be-eq (ir:type expr)
                    (ir:type (ir:ret expr)))))

(defmethod collect-constraints append ((expr ir:fn))
  (list (must-be-eq (ir:type expr)
                    (make-instance 'ir:fn-type
                                   :args (mapcar #'ir:type (ir:arglist expr))
                                   :ret (ir:type (ir:body expr))))))

(defmethod collect-constraints append ((expr ir:quote))
  (list (must-be-eq (ir:type expr)
                    (builtin-type-for-literal (ir:lit expr)))))

(define-special *substitutions* (hash-map symbol ir:type))

(defun find-substitution (type)
  (when (typep type 'ir:type-variable)
    (values (gethash (ir:name type) *substitutions*))))

(typedec #'add-substitution (func (ir:type-variable ir:type) void))
(defun add-substitution (old new)
  (when (ir:same-ident-p old new)
    (return-from add-substitution (values)))
  (if-let ((already-substituded (find-substitution old)))
    (solve-eq-constraint already-substituded new)
    (let* ((better-new (apply-substitutions new)))
      (setf (gethash (ir:name old) *substitutions*) better-new)))
  (values))

(defgeneric solve-constraint (constraint))

(defgeneric solve-eq-constraint (lht rht))

(defmethod solve-constraint ((constraint eq-constraint))
  (with-slot-accessors (lhs rhs) constraint
    (solve-eq-constraint lhs rhs)))

(defmethod solve-eq-constraint (lht rht)
  "fallthrough method"
  (error "unable to unify types ~a with ~a" lht rht))

(defmethod solve-eq-constraint ((lht ir:type-variable) rht)
  (add-substitution lht rht))

(defmethod solve-eq-constraint (lht (rht ir:type-variable))
  (add-substitution rht lht))

(defmethod solve-eq-constraint ((lht ir:fn-type) (rht ir:fn-type))
  (mapc #'solve-eq-constraint (ir:args lht) (ir:args rht))
  (solve-eq-constraint (ir:ret lht) (ir:ret rht)))

(defmethod solve-eq-constraint ((lht ir:primitive-type) (rht ir:primitive-type))
  (assert (ir:same-ident-p lht rht))
  (values))

(defgeneric apply-substitutions (type))

(defmethod apply-substitutions ((old ir:type-variable))
  (if-let ((new (gethash (ir:name old) *substitutions*)))
    (apply-substitutions new)
    old))

(defmethod apply-substitutions ((old ir:type))
  (ir:map-nested-types #'apply-substitutions old))

(defmethod apply-substitutions ((old ir:expr))
  (ir:map-nested-types #'apply-substitutions old))

(defun type-infer (expr)
  (let* ((*substitutions* (make-hash-table :test 'eq))
         (typed (annotate-types expr))
         (constraints (collect-constraints typed)))
    (mapc #'solve-constraint constraints)
    (apply-substitutions typed)))
