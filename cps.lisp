(uiop:define-package #:ploy/cps
  (:use #:ploy/prologue)
  (:import-from #:ploy/builtins
                #:find-builtin)
  (:import-from #:ploy/ir)
  (:export #:cps-transform-program))
(in-package #:ploy/cps)

(defgeneric cps-transform (expr continuation))

(defun cps-transform-program (program)
  (cps-transform program (find-builtin 'ploy-user:|exit|)))

(defmethod cps-transform ((expr ir:call) (cc ir:ident))
  (shallow-copy expr
                :args (cons cc (ir:args expr))))

(defun cont-binding (name)
  (ir:gen-ident (alexandria:symbolicate name '-continuation)))

(defmethod cps-transform ((expr ir:let) (cc ir:ident))
  (with-slot-accessors (ir:binding ir:initform ir:body) expr
    (let* ((continuation (cont-binding 'let)))
      (make-instance 'ir:let
                     :binding continuation
                     :initform (make-instance 'ir:fn
                                              :arglist (list ir:binding)
                                              :body (cps-transform ir:body cc))
                     :body (cps-transform ir:initform continuation)))))

(defmethod cps-transform ((expr ir:prog2) (cc ir:ident))
  (with-slot-accessors (ir:discard ir:ret) expr
    (let* ((ret (cont-binding 'prog2)))
      (make-instance 'ir:let
                     :binding ret
                     :initform (make-instance 'ir:fn
                                              :arglist (list (cont-binding 'discard))
                                              :body (cps-transform ir:ret cc))
                     :body (cps-transform ir:discard ret)))))

(defmethod cps-transform ((expr ir:ident) (cc ir:ident))
  (make-instance 'ir:call
                 :operator cc
                 :args (list expr)))

(defmethod cps-transform ((expr ir:quote) (cc ir:ident))
  (make-instance 'ir:call
                 :operator cc
                 :args (list expr)))

(defmethod cps-transform ((expr ir:fn) (cc ir:ident))
  (let* ((inner-cont (cont-binding 'return))
         (fn-name (ir:gen-ident 'fn)))
    (with-slot-accessors (ir:arglist ir:body) expr
      (make-instance 'ir:let
                     :binding fn-name
                     :initform (make-instance 'ir:fn
                                              :arglist (cons inner-cont ir:arglist)
                                              :body (cps-transform ir:body inner-cont))
                     :body (make-instance 'ir:call
                                          :operator cc
                                          :args (list fn-name))))))
