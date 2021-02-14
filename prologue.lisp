(uiop:define-package ploy/prologue
  (:mix-reexport cl iterate)
  (:import-from gefjon-utils
                define-class define-special
                typedec func void list-of tuple hash-map)
  (:import-from alexandria
                curry rcurry)
  (:export
   name unique-name

   gensymify

   *literal-classes* literal

   ;;; reexports
   ;; gefjon-utils
   define-class define-special
   typedec func void list-of tuple hash-map

   ;; alexandria
   curry rcurry))
(in-package ploy/prologue)

(eval-when (:compile-toplevel :load-toplevel)
  (defun ploy-user-symbol-p (symbol)
    (and (symbolp symbol)
         (eq (symbol-package symbol)
             (find-package 'ploy-user))))
  (defun gensym-p (symbol)
    (and (symbolp symbol)
         (not (symbol-package symbol)))))

(deftype name ()
  '(and symbol (satisfies ploy-user-symbol-p)))

(deftype unique-name ()
  '(and symbol (satisfies gensym-p)))

(typedec #'gensymify (func (&rest (or symbol string)) unique-name))
(defun gensymify (&rest stuff)
  (gensym (format nil "~{~a-~}" stuff)))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *literal-classes* '(fixnum double-float)))

(deftype literal ()
  `(or ,@*literal-classes*))
