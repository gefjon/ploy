(uiop:define-package #:ploy/builtins
  (:use #:ploy/prologue #:cl)
  (:import-from #:ploy/ploy-user
                #:|fixnum| #:|fn| #:|never|)
  (:import-from #:ploy/parse-type
                #:type-scope #:empty-type-scope #:add-type #:parse-type)
  (:import-from #:ploy/ir)
  (:export
   #:*global-type-scope*
   #:enclose-in-builtins
   #:*builtin-terms* #:find-builtin-term
   #:*builtin-types* #:find-builtin-type))
(in-package #:ploy/builtins)

(defmacro define-builtin-types (builtin-types &body names)
  (flet ((construct-type (name)
           `(make-instance 'ir:primitive-type :name ',name)))
    `(progn
       (typedec ,builtin-types (list-of ir:type))
       (defparameter ,builtin-types
         (list ,@(mapcar #'construct-type names))))))

(define-builtin-types *builtin-types*
  ploy-user:|fixnum| ploy-user:|boolean| ploy-user:|never|)

(defparameter *global-type-scope*
  (iter (with scope = (empty-type-scope))
    (for type in *builtin-types*)
    (add-type (ir:name type) type scope)
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
                 (the fixnum (+ lhs rhs)))))

(defun builtin-let-binding (ident &aux (name (ir:name ident)))
  `(,name #',name))

(defun enclose-in-builtins (body)
  `(handler-case
       (let ,(mapcar #'builtin-let-binding *builtin-fns*)
         (declare (ignorable ,@(mapcar #'ir:name *builtin-fns*)))
         ,body)
     (ploy-exit (e) (values-list (return-values e)))))

(typedec #'find-builtin-term (func (name) ir:ident))
(defun find-builtin-term (name)
  (car (or (find name *builtin-terms* :key (~> #'car #'ir:name) :test #'eq)
           (error "unknown builtin fn ~a" name))))

(typedec #'find-builtin-type (func (name) ir:type))
(defun find-builtin-type (name)
  (or (find name *builtin-types* :key #'ir:name :test #'eq)
      (error "unknown builtin type ~a" name)))
