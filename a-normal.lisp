(uiop:define-package #:ploy/a-normal
  (:use #:ploy/prologue)
  (:import-from #:ploy/ir1-expr)
  (:export #:a-normal-transform))
(in-package #:ploy/a-normal)

(defgeneric a-normal-transform (expr))

(defmethod a-normal-transform ((expr ir1:expr))
  (ir1:map-nested-exprs #'a-normal-transform expr))

(defmethod a-normal-transform ((expr ir1:call))
  (with-slot-accessors (ir1:operator ir1:args) expr
    (let* ((new-expr (shallow-copy expr)))
      (iter (with inner = new-expr)
        ;; go right-to-left in-to-out to preserve left-to-right evaluation of arguments
        (for arg in (reverse ir1:args))
        (when (typep arg 'ir1:ident)
          ;; don't transsform args that are already a-normal-ey
          (collect arg into new-arglist at beginning)
          (next-iteration))
        (for arg-binding = (ir1:gen-ident 'arg))
        (collect arg-binding into new-arglist at beginning)
        (setf inner (make-instance 'ir1:let
                                   :binding arg-binding
                                   :initform (a-normal-transform arg)
                                   :body inner))
        (finally
         (setf (ir1:args new-expr) new-arglist)
         (unless (typep ir1:operator 'ir1:ident)
           (let* ((op-binding (ir1:gen-ident 'operator)))
             (setf (ir1:operator new-expr) op-binding
                   inner (make-instance 'ir1:let
                                        :binding op-binding
                                        :initform (a-normal-transform ir1:operator)
                                        :body inner))))
         (return inner))))))

(defmethod a-normal-transform ((expr ir1:macro))
  (error "Macro exists after macroexpansion!"))

(defmethod a-normal-transform ((expr ir1:backquote))
  (error "Backquote exists after macroexpansion!"))

(defmethod a-normal-transform ((expr ir1:comma))
  (error "Comma exists after macroexpansion!"))
