(uiop:define-package ploy/ir1-expr
  (:use ploy/prologue)
  (:nicknames ir1)
  (:shadow prog2 let quote)
  (:import-from closer-mop
                class-slots slot-definition-type slot-value-using-class)
  (:export
   map-nested-exprs

   expr
   prog2 discard ret
   ident name
   let binding initform body
   fn arglist body
   quote lit
   call operator args
   macro arglist body
   backquote term
   comma term))
(in-package ploy/ir1-expr)

(defgeneric map-nested-exprs (function expr))

(define-class expr
    ())

(defmethod map-nested-exprs ((function function) (expr expr))
  (iter (with class = (class-of expr))
    (with new = (shallow-copy expr))
    (for slot in (class-slots (class-of expr)))
    (for slot-type = (slot-definition-type slot))
    (for old-value = (slot-value-using-class class expr slot))
    (cond ((subtypep slot-type 'expr)
            (setf (slot-value-using-class class new slot)
                  (funcall function old-value)))
          ((subtypep slot-type 'sequence)
           (setf (slot-value-using-class class new slot)
                 (map slot-type
                      (lambda (item) (if (typep item 'expr)
                                         (funcall function item)
                                         item))
                      old-value))))
    (finally (return new))))

(define-class prog2
    ((discard expr)
     (ret expr))
  :superclasses (expr))

(define-class ident
    ((name name))
  :superclasses (expr))

(define-class let
    ((binding ident)
     (initform expr)
     (body expr))
  :superclasses (expr))

(define-class fn
    ((arglist (list-of ident))
     (body expr))
  :superclasses (expr))

(define-class quote
    ((lit literal))
  :superclasses (expr))

(define-class call
    ((operator expr)
     (args (list-of ident)))
  :superclasses (expr))

(define-class macro
    ((arglist (list-of ident))
     (body expr))
  :superclasses (expr))

(define-class backquote
    ((term expr))
  :superclasses (expr))

(define-class comma
    ((term expr))
  :superclasses (expr))
