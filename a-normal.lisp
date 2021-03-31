(uiop:define-package #:ploy/a-normal
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir)
  (:export #:a-normal-transform))
(in-package #:ploy/a-normal)

(defgeneric a-normal-transform (expr))

(defmethod a-normal-transform ((expr ir:expr))
  (ir:map-nested-exprs #'a-normal-transform expr))

(defmethod a-normal-transform ((expr ir:call))
  (with-slot-accessors (ir:operator ir:args) expr
    (let* ((new-expr (shallow-copy expr)))
      (iter (with inner = new-expr)
        ;; go right-to-left in-to-out to preserve left-to-right evaluation of arguments
        (for arg in (reverse ir:args))
        (when (typep arg 'ir:ident)
          ;; don't transsform args that are already a-normal-ey
          (collect arg into new-arglist at beginning)
          (next-iteration))
        (for arg-binding = (ir:gen-ident 'arg))
        (collect arg-binding into new-arglist at beginning)
        (setf inner (make-instance 'ir:let
                                   :binding arg-binding
                                   :initform (a-normal-transform arg)
                                   :body inner))
        (finally
         (setf (ir:args new-expr) new-arglist)
         (unless (typep ir:operator 'ir:ident)
           (let* ((op-binding (ir:gen-ident 'operator)))
             (setf (ir:operator new-expr) op-binding
                   inner (make-instance 'ir:let
                                        :binding op-binding
                                        :initform (a-normal-transform ir:operator)
                                        :body inner))))
         (return inner))))))

(defmethod a-normal-transform ((expr ir:macro))
  (error "Macro exists after macroexpansion!"))

(defmethod a-normal-transform ((expr ir:backquote))
  (error "Backquote exists after macroexpansion!"))

(defmethod a-normal-transform ((expr ir:comma))
  (error "Comma exists after macroexpansion!"))
