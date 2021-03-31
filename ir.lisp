(uiop:define-package #:ploy/ir
  (:use #:ploy/prologue)
  (:nicknames #:ir)
  (:shadow #:prog2 #:let #:quote #:type)
  (:import-from #:closer-mop
                #:class-slots #:slot-definition-type #:slot-value-using-class)
  (:export
   #:map-nested-exprs #:map-nested-types

   #:gen-ident #:same-ident-p

   #:type
   #:type-variable #:name
   #:primitive-type #:name
   #:fn-type #:args #:ret
   #:forall-type #:args #:body

   #:expr
   #:prog2 #:discard #:ret
   #:ident #:name
   #:let #:binding #:initform #:body
   #:fn #:arglist #:body
   #:quote #:lit
   #:call #:operator #:args
   #:macro #:arglist #:body
   #:backquote #:term
   #:comma #:term))
(in-package #:ploy/ir)

(defgeneric map-nested-exprs (function expr))

(defgeneric map-nested-types (function type))

(define-class type
    ())

(define-class type-variable
    ((name symbol))
  :superclasses (type))

(define-class primitive-type
    ((name name))
  :superclasses (type))

(define-class fn-type
    ((args (list-of type))
     (ret type))
  :superclasses (type))

(define-class forall-type
    ((args (list-of type-variable))
     (body type))
  :superclasses (type))

(define-class expr
    ((type type)))

(defmethod map-nested-exprs ((function function) (expr expr))
  (labels ((visit (subexpr)
             (etypecase subexpr
               (expr (funcall function subexpr))
               (list (mapcar #'visit subexpr))
               (sequence (map (type-of subexpr) #'visit subexpr))
               (t subexpr))))
    (map-slots #'visit expr)))

(defmethod map-nested-types ((function function) (expr standard-object))
  (labels ((visit (subexpr)
             (etypecase subexpr
               (type (funcall function subexpr))
               (expr (map-nested-types function subexpr))
               (list (mapcar #'visit subexpr))
               (sequence (map (type-of subexpr) #'visit subexpr))
               (t subexpr))))
    (map-slots #'visit expr)))

(define-class prog2
    ((discard expr)
     (ret expr))
  :superclasses (expr))

(define-class ident
    ((name symbol))
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

(defmethod print-object ((ident ident) stream)
  (print-unreadable-object (ident stream :type nil :identity t)
    (write-string (string-downcase (symbol-name (name ident)))
                  stream)))

(defmethod print-object ((let let) stream)
  (print-unreadable-object (let stream :type nil :identity nil)
    (pprint-logical-block (stream nil)
      (write-string "let " stream)
      (write (binding let) :stream stream)
      (write-string " = " stream)
      (pprint-newline :fill stream)
      (write (initform let) :stream stream)
      (write-char #\space stream)
      (pprint-newline :fill stream)
      (write-string "in " stream)
      (write (body let) :stream stream))))

(defmethod print-object ((quote quote) stream)
  (print-object (lit quote) stream))

(defmethod print-object ((call call) stream)
  (write-char #\( stream)
  (pprint-logical-block (stream nil)
    (write (operator call) :stream stream)
    (iter (for arg in (args call))
      (write-char #\space stream)
      (pprint-newline :linear stream)
      (write arg :stream stream))
    (write-char #\) stream)))

(defmethod print-object ((fn fn) stream)
  (print-unreadable-object (fn stream :type nil :identity nil)
    (pprint-logical-block (stream nil)
      (write-string "fn (" stream)
      (pprint-logical-block (stream nil)
        (iter (for arg in (arglist fn))
          (unless (first-time-p)
            (write-char #\space stream)
            (pprint-newline :linear stream))
          (write arg :stream stream)))
      (write-string ") " stream)
      (pprint-newline :fill stream)
      (write-string "-> " stream)
      (write (body fn) :stream stream))))

(typedec #'gen-ident (func (symbol) ir:ident))
(defun gen-ident (name)
  (make-instance 'ir:ident
                 :name (make-gensym name)))

(defun same-ident-p (lhs rhs)
  (eq (ir:name lhs) (ir:name rhs)))
