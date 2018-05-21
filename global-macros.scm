;; the threading macro
(define-macro (-> x . forms)
  (let ((expand-form (lambda (x form)
                       (if (pair? form)
                           (if (or (eq? (car form) 'lambda)
                                   (eq? (car form) 'function))
                               `(,form ,x)
                               `(,(car form) ,x ,@(cdr form)))
                           `(,form ,x)))))
    (reduce #'expand-form x forms)))

;; Reach through objects' slots and extract a value (which obviously
;; breaks the Law of Demeter, but that's besides the point...)
(define (<-% obj slot-names)
  (foldr (lambda (sl o) (slot-ref o sl)) obj (reverse slot-names)))

(define-macro (<- obj . slot-names)
  "Reach through the slots of nested objects, starting with OBJ."
  `(apply <-% (list ,obj (quote ,slot-names))))

;; helper macro to make it more terse to define a method
(define-macro (define-method fn spec args . body)
  `(add-method ,fn (make-method (list ,@spec) (lambda (call-next-method ,@args) ,@body))))
