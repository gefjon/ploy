(uiop:define-package #:ploy/type-infer
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:import-from #:ploy/builtins
                #:find-builtin-type #:*builtin-types*)
  (:export #:type-infer
           #:apply-type-application))
(in-package #:ploy/type-infer)

;;; some forward-decls of generic functions

(defgeneric annotate-types (expr))

(defgeneric collect-constraints (expr)
  (:method-combination append))

(defgeneric solve-constraint (constraint))

(defgeneric solve-eq-constraint (lht rht))

(defgeneric apply-substitutions (type))

(defgeneric free-type-variables (thing))

;;; constraint classes

(define-class constraint ())

(define-class eq-constraint
    ((lhs ir:type)
     (rhs ir:type))
  :superclasses (constraint))

;;; and constructors

(defun must-be-eq (lhs rhs)
  (make-instance 'eq-constraint
                 :lhs lhs
                 :rhs rhs))

;;; special variables for traversals

(define-special *variable-types* (hash-map symbol ir:forall-type))

(defmacro with-variable-types (&body body)
  `(let* ((*variable-types* (make-hash-table :test 'eq)))
     ,@body))

(define-class substitutions
    ((substs (hash-map ir:type ir:type)
             :initform (make-hash-table :test 'eq))
     (parent (or null substitutions))))

(typedec *substitutions* (or null substitutions))
(defparameter *substitutions* (make-instance 'substitutions
                                             :parent nil
                                             :substs *builtin-types*))

(defun call-with-substitutions (thunk)
  (let* ((*substitutions* (make-instance 'substitutions :parent *substitutions*)))
    (funcall thunk)))

(defmacro with-substitutions (&body body)
  `(call-with-substitutions (lambda () ,@body)))

(typedec #'substitute-1 (func (ir:type-variable &optional substitutions)
                              (or null ir:type)))
(defun substitute-1 (old &optional (substitutions *substitutions*))
  (values (gethash old (substs substitutions))))

(typedec #'substitute-var (func (ir:type-variable &optional (or null substitutions))
                                (or null ir:type)))
(defun substitute-var (old &optional (substitutions *substitutions*))
  (when substitutions
    (or (substitute-1 old substitutions)
        (substitute-var old (parent substitutions)))))

;;; instantiating generic types

(defun instantiate (forall)
  (make-instance 'ir:type-application
                 :constructor forall
                 :args (mapcar #'ir:freshen-name (ir:args forall))))

;;; ftv methods

(defmethod free-type-variables ((types list))
  (iter (for type in types)
    (unioning (free-type-variables type)
              test #'ir:same-ident-p)))

(defmethod free-type-variables ((type ir:cl-type))
  (declare (ignorable type))
  nil)

(defmethod free-type-variables ((type ir:type-variable))
  (list type))

(defmethod free-type-variables ((type ir:fn-type))
  (union (free-type-variables (ir:args type))
         (free-type-variables (ir:ret type))
         :test #'ir:same-ident-p))

(defmethod free-type-variables ((type ir:forall-type))
  (set-difference (free-type-variables (ir:body type))
                  (ir:args type)
                  :test #'ir:same-ident-p))

(defmethod free-type-variables ((type ir:type-application))
  (union (free-type-variables (ir:constructor type))
         (free-type-variables (ir:args type))))

;;; adding initial type information to exprs, without constraints or solutions

(defun add-type-variable (expr &optional (name (make-gensym 'type-variable-)))
  (if (ir:type-boundp expr) expr
      (shallow-copy expr
                    :type (make-instance 'ir:type-variable
                                         :name name))))

(defmethod annotate-types ((expr ir:let))
  (let* ((unsolved-initform (annotate-types (ir:initform expr)))
         (init-constraints (collect-constraints unsolved-initform))
         (solved-initform (with-substitutions
                            (mapc #'solve-constraint init-constraints)
                            (apply-substitutions unsolved-initform)))
         (solved-type (ir:type solved-initform))
         (scheme (make-instance 'ir:forall-type
                                :args (free-type-variables solved-type)
                                :body solved-type))
         (initform (shallow-copy solved-initform
                                 :type scheme))
         (binding (shallow-copy (ir:binding expr)
                                :type scheme)))
    (setf (gethash (ir:name (ir:binding expr)) *variable-types*)
          scheme)
    (add-type-variable (shallow-copy expr
                                     :binding binding
                                     :initform initform
                                     :body (annotate-types (ir:body expr))))))

(defmethod annotate-types ((expr ir:expr))
  (ir:map-nested-exprs #'annotate-types (add-type-variable expr)))

(defmethod annotate-types ((expr ir:ident))
  (if-let ((scheme (gethash (ir:name expr) *variable-types*)))
    (shallow-copy expr
                  :type (instantiate scheme))
    (add-type-variable expr)))

;;; finding constraints on exprs

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
  (list (must-be-eq (ir:type expr)
                    (ir:type (ir:body expr)))))

(defmethod collect-constraints append ((expr ir:prog2))
  (list (must-be-eq (ir:type expr)
                    (ir:type (ir:ret expr)))))

(defmethod collect-constraints append ((expr ir:fn))
  (list (must-be-eq (ir:type expr)
                    (make-instance 'ir:fn-type
                                   :args (mapcar #'ir:type (ir:arglist expr))
                                   :ret (ir:type (ir:body expr))))))

(defmethod collect-constraints append ((expr ir:the))
  (list (must-be-eq (ir:type expr)
                    (ir:type (ir:term expr)))))

(defun literal-type (lit)
  (find-builtin-type
   (etypecase lit
     (fixnum 'ploy-user:|fixnum|)
     ((member ploy-user:|true| ploy-user:|false|) 'ploy-user:|boolean|)
     ((eql ploy-user:|nil|) 'ploy-user:|list|))))

(defmethod collect-constraints append ((expr ir:quote))
  (list (must-be-eq (ir:type expr)
                    (literal-type (ir:lit expr)))))

;;; defining and resolving type-substitutions for solutions

(defun find-substitution (type)
  (when (typep type 'ir:type-variable)
    (substitute-var type)))

(typedec #'add-substitution (func (ir:type-variable ir:type) void))
(defun add-substitution (old new)
  (assert *substitutions*)
  (when (ir:same-ident-p old new)
    (return-from add-substitution (values)))
  (if-let ((already-substituded (find-substitution old)))
    (solve-eq-constraint already-substituded new)
    (let* ((better-new (apply-substitutions new)))
      (setf (gethash (ir:name old) (substs *substitutions*)) better-new)))
  (values))

(defmethod apply-substitutions ((old ir:type-variable))
  (if-let ((new (substitute-var old)))
    (apply-substitutions new)
    old))

(defmethod apply-substitutions ((old ir:type))
  (ir:map-nested-types #'apply-substitutions old))

(defmethod apply-substitutions ((old ir:expr))
  (ir:map-nested-types #'apply-substitutions old))

;;; solving constraints

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

(defmethod solve-eq-constraint ((lht ir:cl-type) (rht ir:cl-type))
  (assert (equal (ir:body lht) (ir:body rht)) ()
          "Unable to unify non-EQUAL cl-types ~a and ~a" lht rht)
  (values))

(defmethod solve-eq-constraint ((lht ir:forall-type) rht)
  (solve-eq-constraint (instantiate lht) rht))

(defmethod solve-eq-constraint (lht (rht ir:forall-type))
  (solve-eq-constraint lht (instantiate rht)))

(typedec #'apply-type-application (func (ir:type-application) ir:type))
(defun apply-type-application (type-app)
  ;; FIXME: avoid using enclosing substitutions, which may be from a different scope than where the type
  ;; originates
  (with-slot-accessors (ir:constructor (params ir:args)) type-app
    (let* ((ctor (apply-substitutions ir:constructor)))
      (assert (typep ctor 'ir:forall-type) ()
              "CONSTRUCTOR ~a is not a FORALL-TYPE in TYPE-APPLICATION ~a"
              ctor type-app)
      (with-slot-accessors (ir:body ir:args) ctor
        (assert (= (length params) (length ir:args)))
        (with-substitutions
          (iter (for param in params)
            (for arg in ir:args)
            (add-substitution arg param))
          (apply-substitutions ir:body))))))

(defmethod solve-eq-constraint (lht (rht ir:type-application))
  (solve-eq-constraint lht (apply-type-application rht)))

(defmethod solve-eq-constraint ((lht ir:type-application) rht)
  (solve-eq-constraint (apply-type-application lht) rht))

;;; the external interface

(defun type-infer (expr)
  (with-substitutions
    (with-variable-types
      (let* ((typed (annotate-types expr))
             (constraints (collect-constraints typed)))
        (mapc #'solve-constraint constraints)
        (apply-substitutions typed)))))
