(defsystem "ploy"
  :class :package-inferred-system
  :author "phoebe goldman"
  :version "0.0.1"
  :depends-on ("ploy/package"))

(defsystem "ploy/test"
  :defsystem-depends-on ((:version "fiveam-asdf" "3"))
  :class :package-inferred-fiveam-tester-system
  :depends-on ("ploy/test/package")
  :test-package #:ploy/test/package
  :test-names (#:ploy-toplevel-suite))
