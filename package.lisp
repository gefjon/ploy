(uiop:define-package #:ploy/package
  (:nicknames #:ploy)
  (:shadow #:compile-file)
  (:export #:compile-file)
  (:use #:ploy/prologue #:ploy/reader)
  (:import-from #:ploy/builtins #:enclose-in-builtins #:*global-parser-scope*
                #:ploy-exit #:return-values)
  (:import-from #:ploy/ir)
  (:import-from #:ploy/sexpr-to-ir #:parse-program)
  (:import-from #:ploy/macroexpand #:macroexpand-program)
  (:import-from #:ploy/fixup-macro-shadowing #:fixup-shadowing-in-program)
  (:import-from #:ploy/a-normal #:a-normal-transform)
  (:import-from #:ploy/flatten-anf #:flatten-anf)
  (:import-from #:ploy/ir-to-sexpr #:output-program)
  (:import-from #:ploy/cps #:cps-transform-program)
  (:import-from #:ploy/type-infer #:type-infer))
(in-package #:ploy/package)

(typedec #'ir-transforms (func (ir:expr) ir:expr))
(defun ir-transforms (program)
  (pipe program
    #'macroexpand-program
    #'fixup-shadowing-in-program
    #'a-normal-transform
    #'flatten-anf
    ;; #'cps-transform-program
    #'type-infer))

(typedec #'compile-forms (func (list) list))
(defun compile-forms (program)
  (pipe program
    (rcurry #'parse-program *global-parser-scope*)
    #'ir-transforms
    #'enclose-in-builtins
    #'output-program))

(typedec #'emit-compiled-function (func (list) compiled-function))
(defun emit-compiled-function (compiled-form)
  (values (compile nil `(lambda ()
                          (handler-case
                              ,compiled-form
                            (ploy-exit (e) (values-list (return-values e))))))))

(typedec #'compile-file (func ((or string pathname)) compiled-function))
(defun compile-file (filename)
  (pipe filename
    #'ploy-read-file
    #'compile-forms
    #'emit-compiled-function))
