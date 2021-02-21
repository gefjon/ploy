(uiop:define-package ploy/ir1-passes
  (:use ploy/prologue)
  (:mix-reexport ploy/ir1-passes/sexpr-to-ir1
                 ploy/ir1-passes/macroexpand
                 ploy/ir1-passes/fixup-macro-shadowing
                 ploy/ir1-passes/a-normal)
  (:import-from ploy/ir1-expr)
  (:export run-ir1-passes))
(in-package ploy/ir1-passes)

(typedec #'run-ir1-passes (func (list) ir1:expr))
(defun run-ir1-passes (program)
  (a-normal-transform
   (fixup-shadowing-in-program
    (macroexpand-program
     (parse-program program)))))
