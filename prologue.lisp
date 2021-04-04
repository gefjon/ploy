(uiop:define-package #:ploy/prologue
  (:mix-reexport #:cl #:iterate)
  (:import-from #:gefjon-utils
                #:define-class #:define-special
                #:typedec #:func #:void #:list-of #:tuple #:hash-map
                #:shallow-copy #:with-slot-accessors
                #:~> #:pipe
                #:map-slots
                #:subtypep*)
  (:import-from #:alexandria
                #:curry #:rcurry #:if-let #:when-let #:ensure-gethash #:make-gensym #:symbolicate #:alist-hash-table)
  (:export
   #:gensymify

   #:*literal-classes* #:literal

   ;;; reexports
   ;; gefjon-utils
   #:define-class #:define-special
   #:typedec #:func #:void #:list-of #:tuple #:hash-map
   #:shallow-copy #:with-slot-accessors
   #:~> #:pipe
   #:map-slots
   #:subtypep*
   ;; alexandria
   #:curry #:rcurry #:if-let #:when-let #:ensure-gethash #:make-gensym #:symbolicate #:alist-hash-table))
(in-package #:ploy/prologue)

(eval-when (:compile-toplevel :load-toplevel)
  (defun ploy-user-symbol-p (symbol)
    (and (symbolp symbol)
         (eq (symbol-package symbol)
             (find-package 'ploy-user))))
  (defun gensym-p (symbol)
    (and (symbolp symbol)
         (not (symbol-package symbol)))))

(typedec #'gensymify (func (&rest (or symbol string)) symbol))
(defun gensymify (&rest stuff)
  (gensym (format nil "~{~a-~}" stuff)))

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *literal-classes* '(fixnum double-float)))

(deftype literal ()
  `(or ,@*literal-classes*))
