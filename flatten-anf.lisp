;;; Transform such that no `let''s `initform' is itself a `let'.
;; e.g.:
;; let foo = let bar = baz in froz baz
;;   in froz foo
;; becomes:
;; let bar = baz
;;   in let foo = froz baz
;;     in froz foo
(uiop:define-package #:ploy/flatten-anf
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:export #:flatten-anf))
(in-package #:ploy/flatten-anf)

(defgeneric flatten-anf (expr))

(defgeneric flatten-initform (expr))

(defmethod flatten-initform ((expr ir:let))
  (multiple-value-bind (initform initform-binds) (flatten-initform (ir:initform expr))
    (multiple-value-bind (body body-binds) (flatten-initform (ir:body expr))
      (values body (acons (ir:binding expr) initform
                          (append initform-binds body-binds))))))

(defmethod flatten-initform ((expr ir:expr))
  (values (flatten-anf expr) nil))

(defun enclose-in-bindings (expr bindings)
  (if-let ((binding (first bindings)))
    (let* ((ident (car binding))
           (initform (cdr binding))
           (new-expr (make-instance 'ir:let
                                    :binding ident
                                    :initform initform
                                    :body expr)))
      (enclose-in-bindings new-expr (rest bindings)))
    expr))

(defmethod flatten-anf ((expr ir:let))
  (multiple-value-bind (initform needed-binds) (flatten-initform (ir:initform expr))
    (let* ((new-expr (shallow-copy expr
                                   :initform initform
                                   :body (flatten-anf (ir:body expr)))))
      (enclose-in-bindings new-expr needed-binds))))

(defmethod flatten-anf ((expr ir:expr))
  (ir:map-nested-exprs #'flatten-anf expr))
