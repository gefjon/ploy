(uiop:define-package #:ploy/test/package
  (:mix #:ploy #:fiveam #:cl)
  (:export #:ploy-toplevel-suite))
(in-package #:ploy/test/package)

(def-suite ploy-toplevel-suite)
