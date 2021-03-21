(uiop:define-package #:ploy/builtins
  (:use #:ploy/prologue #:cl)
  (:import-from #:ploy/ploy-user)
  (:import-from #:ploy/ir1-expr)
  (:export
   #:enclose-in-builtins
   #:*builtin-names*
   #:find-builtin))
(in-package #:ploy/builtins)

(defmacro define-builtins (builtin-names &body builtins)
  (flet ((construct-ident (builtin-spec)
           (let* ((name (first builtin-spec)))
             `(make-instance 'ir1:ident :name ',name)))
         (defun-form (builtin-spec)
           (cons 'defun builtin-spec)))
    `(progn
       (typedec ,builtin-names (list-of ir1:ident))
       (defparameter ,builtin-names
         (list ,@(mapcar #'construct-ident builtins)))
       ,@(mapcar #'defun-form builtins))))

(define-class ploy-exit
    ((return-values list))
  :condition t)

(define-builtins *builtin-names*
  (ploy-user:|exit| (&rest stuff)
             (error 'ploy-exit :return-values stuff))
  (ploy-user:+ (cc lhs rhs)
               (funcall cc (+ lhs rhs))))

(defun builtin-let-binding (ident &aux (name (ir1:name ident)))
  `(,name #',name))

(defun enclose-in-builtins (body)
  `(handler-case
       (let ,(mapcar #'builtin-let-binding *builtin-names*)
         (declare (ignorable ,@(mapcar #'ir1:name *builtin-names*)))
         ,body)
     (ploy-exit (e) (values-list (return-values e)))))

(typedec #'find-builtin (func (name) ir1:ident))
(defun find-builtin (name)
  (or (find name *builtin-names* :key #'ir1:name :test #'eq)
      (error "unknown builtin ~a" name)))
