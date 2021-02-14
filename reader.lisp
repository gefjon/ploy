(uiop:define-package ploy/reader
  (:use ploy/prologue)
  (:import-from ploy/ploy-user)
  (:import-from named-readtables
                defreadtable find-readtable)
  (:export ploy-read ploy-read-file))
(in-package ploy/reader)

(defreadtable ploy
  (:merge :common-lisp)
  (:case :preserve))

(typedec #'ploy-read (func (stream) t))
(defun ploy-read (stream)
  (let* ((*package* (find-package :ploy-user))
         (*readtable* (find-readtable 'ploy)))
    (read stream)))

(typedec #'ploy-read-file (func ((or string pathname)) list))
(defun ploy-read-file (filename)
  (let* ((*package* (find-package :ploy-user))
         (*readtable* (find-readtable 'ploy)))
    (iter (for form in-file filename)
      (collect form))))
