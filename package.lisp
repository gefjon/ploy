(uiop:define-package ploy/package
  (:nicknames ploy)
  (:shadow compile-file)
  (:export compile-file)
  (:use ploy/prologue ploy/ir1-passes ploy/reader))
(in-package ploy/package)

(typedec #'compile-file (func ((or string pathname)) ir1:expr))
(defun compile-file (filename)
  (run-ir1-passes (ploy-read-file filename)))
