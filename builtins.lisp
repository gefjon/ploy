(uiop:define-package #:ploy/builtins
  (:use #:ploy/prologue #:cl)
  (:import-from #:ploy/ploy-user
                #:|cl-type|
                #:|fixnum| #:|fn| #:|never| #:|forall|
                #:|list| #:|cons| #:|nil|)
  (:import-from #:ploy/parse-type
                #:type-scope #:empty-type-scope #:add-type #:parse-type)
  (:import-from #:ploy/ir)
  (:export
   #:*global-type-scope*
   #:enclose-in-builtins
   #:*builtin-terms* #:find-builtin-term
   #:*builtin-types* #:parse-builtin-type))
(in-package #:ploy/builtins)

(defmacro define-builtin-types (builtin-types &body names)
  (flet ((make-pair (descriptor)
           `(cons ',(first descriptor)
                  (parse-type (empty-type-scope) ',(second descriptor)))))
    `(progn
       (typedec ,builtin-types (list-of (cons symbol ir:type)))
       (defparameter ,builtin-types
         (list ,@(mapcar #'make-pair names))))))

(define-builtin-types *builtin-types*
  (|fixnum| (|cl-type| fixnum))
  (|boolean| (|cl-type| boolean))
  (|never| (|cl-type| nil))
  (|list| (|forall| (|a|) (|cl-type| list))))

(defparameter *global-type-scope*
  (iter (with scope = (empty-type-scope))
    (for (name . type) in *builtin-types*)
    (add-type name type scope)
    (finally (return scope))))

(defmacro define-builtin-terms (parameter-name &body builtins)
  (flet ((construct-pair (builtin-spec)
           (destructuring-bind (name type value) builtin-spec
             `(cons (make-instance 'ir:ident
                                   :name ',name
                                   :type (parse-type *global-type-scope* ',type))
                    ,value))))
    `(progn
       (typedec ,parameter-name (list-of (cons ir:ident t)))
       (defparameter ,parameter-name
         (list ,@(mapcar #'construct-pair builtins))))))

(define-class ploy-exit
    ((return-values list))
  :condition t)

(define-builtin-terms *builtin-terms*
  (ploy-user:|exit| (|fn| (|fixnum|) |never|)
             (lambda (code)
               (error 'ploy-exit :return-values (list code))))
  (ploy-user:+ (|fn| (|fixnum| |fixnum|) |fixnum|)
               (lambda (lhs rhs)
                 (declare (fixnum lhs rhs))
                 (the fixnum (+ lhs rhs))))
  (ploy-user:|cons| (|forall| (|a|) (|fn| (|a| (|list| |a|)) (|list| |a|)))
             (lambda (elt list)
               (cons elt list))))

(defun builtin-let-binding (ident)
  `(,(ir:name (car ident)) ,(cdr ident)))

(defun enclose-in-builtins (body)
  `(handler-case
       (let ,(mapcar #'builtin-let-binding *builtin-terms*)
         (declare (ignorable ,@(mapcar (~> #'car #'ir:name) *builtin-terms*)))
         ,body)
     (ploy-exit (e) (values-list (return-values e)))))

(typedec #'find-builtin-term (func (symbol) ir:ident))
(defun find-builtin-term (name)
  (car (or (find name *builtin-terms* :key (~> #'car #'ir:name) :test #'eq)
           (error "unknown builtin fn ~a" name))))

(typedec #'parse-builtin-type (func (t) ir:type))
(defun parse-builtin-type (type)
  (parse-type *global-type-scope* type))
