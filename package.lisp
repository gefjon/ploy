(uiop:define-package ploy/package
  (:nicknames ploy)
  (:shadow compile-file)
  (:export compile-file)
  (:use ploy/prologue ploy/reader)
  (:import-from ploy/ir1-expr)
  (:import-from ploy/sexpr-to-ir1 parse-program)
  (:import-from ploy/macroexpand macroexpand-program)
  (:import-from ploy/fixup-macro-shadowing fixup-shadowing-in-program)
  (:import-from ploy/a-normal a-normal-transform)
  (:import-from ploy/flatten-a-normal flatten-anf)
  (:import-from ploy/ir1-to-sexpr output-program))
(in-package ploy/package)

(typedec #'ir1-transforms (func (ir1:expr) ir1:expr))
(defun ir1-transforms (program)
  (pipe program
    #'macroexpand-program
    #'fixup-shadowing-in-program
    #'a-normal-transform
    #'flatten-anf))

(typedec #'compile-forms (func (list) list))
(defun compile-forms (program)
  (pipe program
    #'parse-program
    #'ir1-transforms
    #'output-program))

(typedec #'emit-compiled-function (func (list) compiled-function))
(defun emit-compiled-function (compiled-form)
  (values (compile nil `(lambda ()
                          (block program
                            (let ((ploy/builtins:+ (lambda (lhs rhs)
                                                      (+ lhs rhs)))
                                  (ploy/builtins:exit (lambda (thing)
                                                        (return-from program thing))))
                              (declare (ignorable ploy/builtins:exit))
                              ,compiled-form))))))

(typedec #'compile-file (func ((or string pathname)) compiled-function))
(defun compile-file (filename)
  (pipe filename
    #'ploy-read-file
    #'compile-forms
    #'emit-compiled-function))
