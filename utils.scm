(define (find item seq)
  (let ((x (member item seq)))
    (if x (cadr x) '())))

(define (random-elt l) (list-ref l (random-integer (length l))))

(define (lerp a b t) (+ (* (- 1 t) a) (* t b)))

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
