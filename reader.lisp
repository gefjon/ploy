(uiop:define-package ploy/reader
  (:use ploy/prologue)
  (:import-from ploy/ploy-user)
  (:import-from named-readtables
                defreadtable find-readtable)
  (:export ploy-read ploy-read-file))
(in-package ploy/reader)

(typedec #'reader-fn (func (name) (func (stream character) list)))
(defun reader-fn (symbol)
  (lambda (stream char)
    (declare (ignore char))
    (list symbol (ploy-read stream :recursive-p t))))

(defreadtable ploy
  (:merge :common-lisp)
  (:case :preserve)
  (:macro-char #\` (reader-fn 'ploy-user:|backquote|))
  (:macro-char #\, (reader-fn 'ploy-user:|comma|))
  (:macro-char #\' (reader-fn 'ploy-user:|quote|)))

(typedec #'ploy-read (func (stream &key (:eof-error-p boolean)
                                   (:eof-value t)
                                   (:recursive-p boolean))
                           t))
(defun ploy-read (stream &key (eof-error-p t)
                           eof-value
                           recursive-p)
  (let* ((*package* (find-package :ploy-user))
         (*readtable* (find-readtable 'ploy)))
    (read stream eof-error-p eof-value recursive-p)))

(typedec #'ploy-read-file (func ((or string pathname)) list))
(defun ploy-read-file (filename)
  (let* ((*package* (find-package :ploy-user))
         (*readtable* (find-readtable 'ploy)))
    (iter (for form in-file filename)
      (collect form))))
