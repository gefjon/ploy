(uiop:define-package :ploy/builtins
  (:use :ploy/prologue :ploy/sexpr-to-ir :cl)
  (:import-from :ploy/ploy-user
                #:|cl-type|
                #:|fixnum| #:|fn| #:|never| #:|forall|
                #:|list| #:|cons| #:|nil|)
  (:import-from :ploy/ir)
  (:export
   #:ploy-exit #:return-values
   #:*global-type-scope*
   #:enclose-in-builtins
   #:*builtin-terms*
   #:*builtin-types*
   #:find-builtin-type #:find-builtin-term))
(in-package :ploy/builtins)

(defmacro define-builtin-types (builtin-types &body names)
  (flet ((make-pair (descriptor)
           `(cons ',(first descriptor)
                  '(parse-type ),(second descriptor))))
    `(progn
       (typedec ,builtin-types (list-of (cons symbol ir:type)))
       (defparameter ,builtin-types
         (list ,@(mapcar #'make-pair names))))))

(defparameter *global-parser-scope* (make-instance 'parser-scope
                                            :name 'global-scope
                                            :parent nil))

(typedec *builtin-types* (hash-map ir:type-variable ir:type))
(defparameter *builtin-types* (make-hash-table :test 'eq))

(typedec *builtin-terms* (hash-map ir:ident (cons ir:type t)))
(defparameter *builtin-terms* (make-hash-table :test 'eq))

(typedec #'%define-builtin-type (func (symbol (or symbol list)) ir:type))
(defun %define-builtin-type (name expansion)
  (let* ((ident (find-type name *global-parser-scope*)))
    (setf (gethash ident *builtin-types*)
          (parse-type *empty-parser-scope* expansion))))

(defmacro define-builtin-type (name expansion)
  `(%define-builtin-type ',name ',expansion))

(define-builtin-type |fixnum| (|cl-type| fixnum))
(define-builtin-type |boolean| (|cl-type| boolean))
(define-builtin-type |never| (|cl-type| nil))
(define-builtin-type |list| (|forall| (|a|) (|cl-type| list)))

(typedec #'%define-builtin-term (func (symbol (or symbol list) t) (cons ir:type t)))
(defun %define-builtin-term (name type-form value)
  (let* ((ident (find-ident name *global-parser-scope*))
         (type (parse-type *global-parser-scope* type-form)))
    (setf (gethash ident *builtin-terms*)
          (cons type value))))

(defmacro define-builtin-term (name type value)
  `(%define-builtin-term ',name ',type ,value))

(define-class ploy-exit
    ((return-values list))
  :condition t)

(define-builtin-term ploy-user:|exit|
  (|fn| (|fixnum|) |never|)
  (lambda (code)
    (error 'ploy-exit :return-values (list code))))
(define-builtin-term ploy-user:+
  (|fn| (|fixnum| |fixnum|) |fixnum|)
  (lambda (lhs rhs)
    (declare (fixnum lhs rhs))
    (the fixnum (+ lhs rhs))))
(define-builtin-term ploy-user:|cons|
  (|forall| (|a|) (|fn| (|a| (|list| |a|)) (|list| |a|)))
  (lambda (elt list)
    (cons elt list)))

(typedec #'enclose-in-builtins (func (ir:expr) ir:expr))
(defun enclose-in-builtins (body)
  (iter (with inner = body)
    (for (name (type . initform)) in-hashtable *builtin-terms*)
    (setf inner (make-instance 'ir:let
                               :body inner
                               :initform (make-instance 'ir:quote
                                                        :type type
                                                        :lit initform)
                               :binding name
                               :type (ir:type inner)
                               :ignorablep t))
    (finally (return inner))))

(typedec #'find-builtin-type (func (symbol) ir:type-variable))
(defun find-builtin-type (name)
  (find-type name *global-parser-scope*))

(typedec #'find-builtin-term (func (symbol) ir:ident))
(defun find-builtin-term (name)
  (find-ident name *global-parser-scope*))
