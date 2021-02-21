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
  (pprint-logical-block (stream nil)
    (write-char #\( stream)
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
