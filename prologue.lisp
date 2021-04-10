(uiop:define-package #:ploy/prologue
  (:mix-reexport #:cl #:iterate)
  (:import-from #:gefjon-utils
                #:define-class #:define-special
                #:typedec #:func #:void #:list-of #:tuple #:hash-map
                #:shallow-copy #:with-slot-accessors
                #:~> #:pipe
                #:map-slots
                #:subtypep*)
  (:import-from #:ploy/ploy-user)
  (:import-from #:alexandria
                #:curry #:rcurry #:if-let #:when-let #:ensure-gethash #:make-gensym #:symbolicate #:alist-hash-table)
  (:export
   #:gensymify

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

(typedec #'gensymify (func (&rest (or symbol string)) symbol))
(defun gensymify (&rest stuff)
  (gensym (format nil "~{~a-~}" stuff)))
