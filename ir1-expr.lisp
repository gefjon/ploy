(uiop:define-package ploy/ir1-expr
  (:use ploy/prologue)
  (:nicknames ir1)
  (:shadow prog2 let quote)
  (:export
   expr
   prog2 discard ret
   ident name
   let binding initform body
   fn arglist body
   quote lit
   call operator args
   macro arglist body))
(in-package ploy/ir1-expr)

(define-class expr
    ())

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
    ((operator ident)
     (args (list-of ident)))
  :superclasses (expr))

(define-class macro
    ((arglist (list-of ident))
     (body expr))
  :superclasses (expr))
