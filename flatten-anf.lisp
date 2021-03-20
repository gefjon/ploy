;;; Transform such that no `let''s `initform' is itself a `let'.
;; e.g.:
;; let foo = let bar = baz in froz baz
;;   in froz foo
;; becomes:
;; let bar = baz
;;   in let foo = froz baz
;;     in froz foo
(uiop:define-package ploy/flatten-a-normal
  (:use ploy/prologue)
  (:import-from ploy/ir1-expr)
  (:export flatten-anf))
(in-package ploy/flatten-a-normal)

(defgeneric flatten-anf (expr))

(defgeneric flatten-initform (expr))

(defmethod flatten-initform ((expr ir1:let))
  (multiple-value-bind (initform initform-binds) (flatten-initform (ir1:initform expr))
    (multiple-value-bind (body body-binds) (flatten-initform (ir1:body expr))
      (values body (acons (ir1:binding expr) initform
                          (append initform-binds body-binds))))))

(defmethod flatten-initform ((expr ir1:expr))
  (values (flatten-anf expr) nil))

(defun enclose-in-bindings (expr bindings)
  (if-let ((binding (first bindings)))
    (let* ((ident (car binding))
           (initform (cdr binding))
           (new-expr (make-instance 'ir1:let
                                    :binding ident
                                    :initform initform
                                    :body expr)))
      (enclose-in-bindings new-expr (rest bindings)))
    expr))

(defmethod flatten-anf ((expr ir1:let))
  (multiple-value-bind (initform needed-binds) (flatten-initform (ir1:initform expr))
    (let* ((new-expr (shallow-copy expr
                                   :initform initform
                                   :body (flatten-anf (ir1:body expr)))))
      (enclose-in-bindings new-expr needed-binds))))

(defmethod flatten-anf ((expr ir1:expr))
  (ir1:map-nested-exprs #'flatten-anf expr))
