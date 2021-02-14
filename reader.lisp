(uiop:define-package ploy/reader
  (:use ploy/prologue)
  (:import-from ploy/ploy-user)
  (:import-from named-readtables
                defreadtable find-readtable)
  (:export ploy-read))
(in-package ploy/reader)

(defreadtable ploy
  (:merge :common-lisp)
  (:case :preserve))

(typedec #'ploy-read (func (stream) list))
(defun ploy-read (stream)
  (let* ((*package* (find-package :ploy-user))
         (*readtable* (find-readtable 'ploy)))
    (read stream)))
