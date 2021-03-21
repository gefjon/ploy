(uiop:define-package #:ploy/cps
  (:use #:ploy/prologue)
  (:import-from #:ploy/builtins
                #:find-builtin)
  (:import-from #:ploy/ir1-expr)
  (:export #:cps-transform-program))
(in-package #:ploy/cps)

(defgeneric cps-transform (expr continuation))

(defun cps-transform-program (program)
  (cps-transform program (find-builtin 'ploy-user:|exit|)))

(defmethod cps-transform ((expr ir1:call) (cc ir1:ident))
  (shallow-copy expr
                :args (cons cc (ir1:args expr))))

(defun cont-binding (name)
  (ir1:gen-ident (alexandria:symbolicate name '-continuation)))

(defmethod cps-transform ((expr ir1:let) (cc ir1:ident))
  (with-slot-accessors (ir1:binding ir1:initform ir1:body) expr
    (let* ((continuation (cont-binding 'let)))
      (make-instance 'ir1:let
                     :binding continuation
                     :initform (make-instance 'ir1:fn
                                              :arglist (list ir1:binding)
                                              :body (cps-transform ir1:body cc))
                     :body (cps-transform ir1:initform continuation)))))

(defmethod cps-transform ((expr ir1:prog2) (cc ir1:ident))
  (with-slot-accessors (ir1:discard ir1:ret) expr
    (let* ((ret (cont-binding 'prog2)))
      (make-instance 'ir1:let
                     :binding ret
                     :initform (make-instance 'ir1:fn
                                              :arglist (list (cont-binding 'discard))
                                              :body (cps-transform ir1:ret cc))
                     :body (cps-transform ir1:discard ret)))))

(defmethod cps-transform ((expr ir1:ident) (cc ir1:ident))
  (make-instance 'ir1:call
                 :operator cc
                 :args (list expr)))

(defmethod cps-transform ((expr ir1:quote) (cc ir1:ident))
  (make-instance 'ir1:call
                 :operator cc
                 :args (list expr)))

(defmethod cps-transform ((expr ir1:fn) (cc ir1:ident))
  (let* ((inner-cont (cont-binding 'return))
         (fn-name (ir1:gen-ident 'fn)))
    (with-slot-accessors (ir1:arglist ir1:body) expr
      (make-instance 'ir1:let
                     :binding fn-name
                     :initform (make-instance 'ir1:fn
                                              :arglist (cons inner-cont ir1:arglist)
                                              :body (cps-transform ir1:body inner-cont))
                     :body (make-instance 'ir1:call
                                          :operator cc
                                          :args (list fn-name))))))
