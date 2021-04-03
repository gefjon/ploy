(uiop:define-package #:ploy/parse-type
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:export #:type-scope #:emtpy-type-scope #:parse-type #:make-type #:find-type #:add-type))
(in-package #:ploy/parse-type)

(define-class type-scope
    ((types (hash-map name ir:type)
            :initform (make-hash-table :test 'eq))
     (parent (or null scope))))

(typedec #'add-type (func (name ir:type type-scope) ir:type))
(defun add-type (name type scope)
  (setf (gethash name (types scope))
        type))

(typedec #'make-type (func (name type-scope &optional (or symbol class)) ir:type))
(defun make-type (name scope &optional (class 'ir:type-variable) )
  (assert (subtypep* class 'ir:type))
  (add-type name
            (make-instance class
                              :name name)
            scope))

(defun empty-type-scope ()
  (make-instance 'type-scope
                 :parent nil))

(typedec #'find-type (func (name type-scope) ir:type))
(defun find-type (name scope)
  (or (gethash name (types scope))
      (and (parent scope) (find-type name (parent scope)))
      (error "unknown type ~a" name)))

(defgeneric parse-type (current-scope form))

(defgeneric parse-type-form (current-scope head &rest tail))

(defmethod parse-type ((scope type-scope) (form list))
  (apply #'parse-type-form scope form))

(defmethod parse-type ((scope type-scope) (form symbol))
  (find-type form scope))

(defmacro define-type ((head &rest arglist) (scope) &body body)
  `(defmethod parse-type-form ((,scope type-scope) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(define-type (ploy-user:|fn| args ret) (scope)
  (make-instance 'ir:fn-type
                 :args (mapcar (curry #'parse-type scope) args)
                 :ret (parse-type scope ret)))

