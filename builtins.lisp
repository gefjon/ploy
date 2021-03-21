(uiop:define-package #:ploy/builtins
  (:use #:ploy/prologue)
  (:import-from #:ploy/ploy-user)
  (:import-from #:ploy/ir1-expr)
  (:shadow #:+)
  (:export
   #:*builtin-names*
   
   #:*exit* #:exit
   #:*+* #:+))
(in-package ploy/builtins)

(defparameter *exit* (make-instance 'ir1:ident
                                    :name 'ploy-user:|exit|))

(defparameter *+* (make-instance 'ir1:ident
                                 :name 'ploy-user:+))

(defun + (cc lhs rhs)
  (funcall cc (cl:+ lhs rhs)))

(defparameter *builtin-names* `((,*exit* . exit)
                                (,*+* . +)))
