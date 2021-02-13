(uiop:define-package ploy/ploy
  (:nicknames ploy)
  (:import-from gefjon-utils
                define-class define-enum define-special
                typedec func void list-of hash-map tuple)
  (:import-from alexandria
                curry with-gensyms make-gensym symbolicate)
  (:mix cl iterate))
(in-package ploy)

(uiop:define-package ploy-user
  (:mix)
  (:import-from cl
                nil +)
  (:export
   let scope

   nil +))

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

(defgeneric compile-expr (scope remaining-body form))

(defgeneric compile-list-expr (scope remaining-body head &rest tail))

(defmethod compile-expr ((scope symbol) (remaining-body list) (form cons))
  (apply #'compile-list-expr scope remaining-body form))

(defmacro define-compile-literals (&rest literal-classes)
  (cons 'progn
        (iter (for literal in literal-classes)
          (collect `(defmethod compile-expr ((scope symbol) (remaining-body null) (literal ,literal))
                      ,(format nil "Treat a ~a as a literal, returning it" literal)
                      (declare (ignorable scope remaining-body))
                      (list 'quote literal))))))

(define-compile-literals fixnum double-float)

(defmethod compile-expr ((scope symbol) (remaining-body null) (form symbol))
  `(scope-read ,scope ,form))

(defmethod compile-list-expr ((scope symbol) (remaining-body list) function &rest arglist)
  "Fallthrough: compile as a function call"
  (let* ((funcall `(funcall ,(compile-expr scope nil function)
                            ,@(mapcar (curry #'compile-expr scope nil) arglist))))
    (if remaining-body
        `(progn ,funcall ,(compile-expr scope (rest remaining-body) (first remaining-body)))
        funcall)))

(defmacro define-expr ((head &rest arglist) (scope remaining-body) &body body)
  `(defmethod compile-list-expr
       ((,scope symbol) (,remaining-body list) (head (eql ',head)) &rest tail)
     (destructuring-bind ,arglist tail
       ,@body)))

(typedec #'scoped-lambda (func (symbol symbol (list-of symbol) list) (values symbol t)))
(defun scoped-lambda (enclosing-scope fn-name arglist body)
  (let* ((body-scope (gensymify 'body fn-name))
         (body (compile-expr body-scope (rest body) (first body)))
         (arg-unique-names (mapcar (curry #'gensymify 'arg) arglist)))
    (values fn-name
            `(lambda ,arg-unique-names
               (ploy-user:scope ,body-scope ,enclosing-scope
                                ,(mapcar #'list arglist arg-unique-names)
                                ,@body)))))

(typedec #'let-binding-name-initform (func (symbol (or symbol (list-of symbol)) list) (values symbol t)))
(defun let-binding-name-initform (enclosing-scope name-or-function value-or-body)
  (etypecase name-or-function
    (list (let* ((name (first name-or-function))
                 (arglist (rest name-or-function)))
            (scoped-lambda enclosing-scope name arglist value-or-body)))
    (symbol (assert (= (length value-or-body) 1) ()
                    "Malformed non-function let ~a should have exactly one value form but found ~a"
                    name-or-function value-or-body)
     (values name-or-function (compile-expr enclosing-scope nil (first value-or-body))))))

(define-expr (ploy-user:let name-or-function &body value-or-body) (enclosing-scope remaining-body)
  (assert remaining-body ()
          "let-binding unused: ~a" name-or-function)
  (multiple-value-bind (name initform)
      (let-binding-name-initform enclosing-scope name-or-function value-or-body)
    (let* ((scope-name (gensymify 'let name)))
      `(ploy-user:scope ,scope-name ,enclosing-scope ((,name ,initform))
              ,(compile-expr scope-name (rest remaining-body) (first remaining-body))))))
