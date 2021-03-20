(uiop:define-package ploy/fixup-macro-shadowing
  (:use ploy/prologue)
  (:import-from ploy/ir1-expr)
  (:export fixup-shadowing-in-program))
(in-package ploy/fixup-macro-shadowing)

(defgeneric make-shadowed-idents-eq (substitutions expr))

(defmethod make-shadowed-idents-eq ((substitutions list) (expr ir1:expr))
  (ir1:map-nested-exprs (curry #'make-shadowed-idents-eq substitutions) expr))


(defmethod make-shadowed-idents-eq ((substitutions list) (expr ir1:ident))
  (if-let ((subst (cdr (assoc expr substitutions))))
    (make-shadowed-idents-eq substitutions subst)
    expr))

(defmethod make-shadowed-idents-eq ((substitutions list) (expr ir1:let))
  (with-slot-accessors (ir1:binding ir1:body) expr
    (let* ((new-ident (shallow-copy ir1:binding))
           (inner-substs (acons ir1:binding new-ident substitutions)))
      (shallow-copy expr
                    :binding new-ident
                    :body (make-shadowed-idents-eq inner-substs ir1:body)))))

(defmethod make-shadowed-idents-eq ((substitutions list) (expr ir1:fn))
  (with-slot-accessors (ir1:arglist ir1:body) expr
    (let* ((new-arglist (mapcar #'shallow-copy ir1:arglist))
           (inner-substs (pairlis ir1:arglist new-arglist substitutions)))
      (shallow-copy expr
                    :arglist new-arglist
                    :body (make-shadowed-idents-eq inner-substs ir1:body)))))

(typedec #'fixup-shadowing-in-program (func (ir1:expr) ir1:expr))
(defun fixup-shadowing-in-program (program)
  (make-shadowed-idents-eq nil program))
