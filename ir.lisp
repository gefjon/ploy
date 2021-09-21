(uiop:define-package #:ploy/ir
  (:use #:ploy/prologue)
  (:nicknames #:ir)
  (:shadow #:prog2 #:let #:quote #:type #:if #:else)
  (:import-from #:closer-mop
                #:class-slots #:slot-definition-type #:slot-value-using-class)
  (:export
   #:map-nested-exprs #:map-nested-types

   #:gen-ident #:freshen-name #:same-ident-p

   #:type
   #:type-variable #:name
   #:cl-type #:body
   #:fn-type #:args #:ret
   #:forall-type #:args #:body
   #:type-application #:constructor #:args

   #:expr #:type #:type-boundp
   #:if #:predicate #:then #:else
   #:prog2 #:discard #:ret
   #:ident #:name
   #:let #:binding #:initform #:ignorablep #:body
   #:fn #:arglist #:body
   #:quote #:lit
   #:call #:operator #:args
   #:macro #:arglist #:body
   #:backquote #:term
   #:comma #:term))
(in-package #:ploy/ir)

;;; visitor interfaces

(defgeneric map-nested-exprs (function expr))

(defgeneric map-nested-types (function type))

;;; abstract classes

(define-class type ())

(define-class expr
    ((type type)))

(define-class name
  ((name symbol :initform (error "unbound name"))))

;;; type variants

(define-class type-variable ()
  :superclasses (type name))

(define-class cl-type
    ((body t))
  :superclasses (type))

(define-class fn-type
    ((args (list-of type))
     (ret type))
  :superclasses (type))

(define-class forall-type
    ((args (list-of type-variable))
     (body type))
  :superclasses (type))

(define-class type-application
    ((constructor type)
     (args (list-of type)))
  :superclasses (type))

;;; visitor methods

(defmethod map-nested-exprs ((function function) (expr expr))
  (labels ((visit (subexpr)
             (etypecase subexpr
               (expr (funcall function subexpr))
               (list (mapcar #'visit subexpr))
               (t subexpr))))
    (map-slots #'visit expr)))

(defmethod map-nested-types ((function function) (expr standard-object))
  (labels ((visit (subexpr)
             (etypecase subexpr
               (type (funcall function subexpr))
               (expr (map-nested-types function subexpr))
               (list (mapcar #'visit subexpr))
               (t subexpr))))
    (map-slots #'visit expr)))

;;; expr variants

(define-class if
    ((predicate expr)
     (then expr)
     (else expr))
  :superclasses (expr))

(define-class prog2
    ((discard expr)
     (ret expr))
  :superclasses (expr))

(define-class ident ()
  :superclasses (expr name))

(define-class let
    ((binding ident)
     (initform expr)
     (ignorablep boolean
                 :initform nil)
     (body expr))
  :superclasses (expr))

(define-class fn
    ((arglist (list-of ident))
     (body expr))
  :superclasses (expr))

(define-class call
    ((operator expr)
     (args (list-of ident)))
  :superclasses (expr))

(define-class macro
    ((arglist (list-of ident))
     (body expr))
  :superclasses (expr))

(define-class quote
    ((lit t))
  :superclasses (expr))

(define-class backquote
    ((term expr))
  :superclasses (expr))

(define-class comma
    ((term expr))
  :superclasses (expr))

;;; print-object methods

(defmethod print-object ((name name) stream)
  (print-unreadable-object (name stream :type nil :identity nil)
    (write-string (symbol-name (name name))
                  stream)))

(defmethod print-object ((fn fn-type) stream)
  (print-unreadable-object (fn stream :type nil :identity nil)
    (write-string "fn " stream)
    (pprint-logical-block (stream nil)
      (write-char #\( stream)
      (pprint-logical-block (stream nil)
        (iter (for arg in (args fn))
          (unless (first-time-p)
            (write-char #\space stream)
            (pprint-newline :fill stream))
          (write arg :stream stream))
        (write-char #\) stream))
      (write-char #\space stream)
      (pprint-newline :linear stream)
      (write (ret fn) :stream stream))))

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
  (print-unreadable-object (quote stream :type nil :identity nil)
    (print-object (lit quote) stream)))

(defmethod print-object ((call call) stream)
  (print-unreadable-object (call stream :type nil :identity nil)
    (write-char #\( stream)
    (pprint-logical-block (stream nil)
      (write (operator call) :stream stream)
      (iter (for arg in (args call))
        (write-char #\space stream)
        (pprint-newline :linear stream)
        (write arg :stream stream))
      (write-char #\) stream))))

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

;;; ident-related utilities (some also apply to other names, like type variables)

(typedec #'gen-ident (func (symbol) ir:ident))
(defun gen-ident (name)
  (make-instance 'ir:ident
                 :name (make-gensym name)))

(typedec #'freshen-name (func (name) name))
(defun freshen-name (name)
  (shallow-copy name
                :name (make-gensym (name name))))

(typedec #'same-ident-p (func (t t) boolean))
(defun same-ident-p (lhs rhs)
  (and (eq (class-of lhs) (class-of rhs))
       (subtypep (class-of lhs) (find-class 'name))
       (eq (ir:name lhs) (ir:name rhs))))
