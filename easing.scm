(define (smoothstep x) (* x x (- 3 (* 2 x))))
(define (smootherstep x) (* x x x (+ (* x (- (* x 6) 15)) 10)))

;; linear
(define (lerp a b t d) (let ((b (- b a))) (+ (* b (/ t d)) a)))

;; smoothed out lerp
;; (define (lerp a b t)
;;   (let ((b (- b a))
;;         (t (smootherstep t)))
;;     (+ (* b t) a)))

(define (easein-quad b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 2)) b)))

(define (easein-cubic b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 3)) b)))

(define (easein-quart b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 4)) b)))

(define (easein-fifth b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 5)) b)))

(define (easeout-quad b c t d)
  (let ((c (- c b)))
    (+ (* (- c) (* (/ t d) (- ( t d) 2))) b)))

(define (easeout-cubic b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 3)) b)))

(define (easeout-quart b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 4)) b)))

(define (easeout-fifth b c t d)
  (let ((c (- c b)))
    (+ (* c (expt (/ t d) 5)) b)))

(define (easeinout-quad b c t d)
  (let ((t (/ t (/ d 2)))
        (c (- c b)))
    (if (< t 1)
        (+ b (* (/ c 2) t t))
        (+ b (* (- (/ c 2)) (- (* (- t 1) (- t 3)) 1))))))
