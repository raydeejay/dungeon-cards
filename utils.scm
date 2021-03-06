(define (find item seq)
  (let ((x (member item seq)))
    (if x (cadr x) '())))

(define (random-elt l) (list-ref l (random-integer (length l))))

(define (nth n l)
  (if (or (>= n (length l)) (< n 0))
      (error "Index out of bounds.")
      (if (eq? n 0)
          (car l)
          (nth (- n 1) (cdr l)))))

;; this does foldr
;; (define (reduce fn init list)
;;   (if (null? list)
;;       init
;;       (fn (car list) (reduce fn init (cdr list)))))

(define (reduce fn init list)
  (if (null? list)
      init
      (reduce fn (fn init (car list)) (cdr list))))

(define (bound?% name)
  (not (##unbound? (##global-var-ref (##make-global-var name)))))

(define-macro (bound name)
  `(bound?% ',name))
