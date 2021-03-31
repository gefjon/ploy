(uiop:define-package #:ploy/fixup-macro-shadowing
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:export #:fixup-shadowing-in-program))
(in-package #:ploy/fixup-macro-shadowing)

(defun rename-ident (ident)
  (ir:gen-ident (ir:name ident)))

(defgeneric rename-unique-bindings (substitutions expr))

(defmethod rename-unique-bindings ((substitutions list) (expr ir:expr))
  (ir:map-nested-exprs (curry #'rename-unique-bindings substitutions) expr))


(defmethod rename-unique-bindings ((substitutions list) (expr ir:ident))
  (if-let ((subst (cdr (assoc expr substitutions :key #'ir:name :test #'eq))))
    (rename-unique-bindings substitutions subst)
    expr))

(defmethod rename-unique-bindings ((substitutions list) (expr ir:let))
  (with-slot-accessors (ir:binding ir:body) expr
    (let* ((new-ident (shallow-copy ir:binding))
           (inner-substs (acons ir:binding new-ident substitutions)))
      (shallow-copy expr
                    :binding new-ident
                    :body (rename-unique-bindings inner-substs ir:body)))))

(defmethod rename-unique-bindings ((substitutions list) (expr ir:fn))
  (with-slot-accessors (ir:arglist ir:body) expr
    (let* ((new-arglist (mapcar #'shallow-copy ir:arglist))
           (inner-substs (pairlis ir:arglist new-arglist substitutions)))
      (shallow-copy expr
                    :arglist new-arglist
                    :body (rename-unique-bindings inner-substs ir:body)))))

(typedec #'fixup-shadowing-in-program (func (ir:expr) ir:expr))
(defun fixup-shadowing-in-program (program)
  (rename-unique-bindings nil program))
