;; --------------------
;; game state

(define *valid-moves*
  '(0 (1 3)
    1 (0 2 4)
    2 (1 5)
    3 (0 4 6)
    4 (1 3 5 7)
    5 (2 4 8)
    6 (3 7)
    7 (6 4 8)
    8 (7 5)))

(define (xy->slot x y)
  (cond ((< (- x 15) 120)
         (cond ((< (- y 60) 160)        0)
               ((< (- y 60) (+ 320 5))  3)
               ((< (- y 60) (+ 480 10)) 6)
               (#t 0)))
        ((< (- x 15) (+ 240 5))
         (cond ((< (- y 60) 160)        1)
               ((< (- y 60) (+ 320 5))  4)
               ((< (- y 60) (+ 480 10)) 7)
               (#t 0)))
        ((< (- x 15) (+ 360 10))
         (cond ((< (- y 60) 160)        2)
               ((< (- y 60) (+ 320 5))  5)
               ((< (- y 60) (+ 480 10)) 8)
               (#t 0)))))

(define (slot->x slot)
  (+ 15
     (case slot
       ((0 3 6) 0)
       ((1 4 7) (+ 120 5))
       ((2 5 8) (+ 240 10)))))

(define (slot->y slot)
  (+ 60
     (case slot
       ((0 1 2) 0)
       ((3 4 5) (+ 160 5))
       ((6 7 8) (+ 320 10)))))

(define *field* (make-vector 9))
(define *hero* (make <hero>))

(define (can-interact? from to)
  (member to (find from *valid-moves*)))

(define (move-card from to)
  (vector-set! *field* to (vector-ref *field* from))
  (vector-set! *field* from (generate-card)))

(define (move-to-target from to)
  (vector-set! *field* to *hero*)
  (set! hero-cell to)
  (vector-set! *field* from (generate-card)))

(define (generate-field)
  (let ((v (make-vector 9)))
    (do ((i 0 (+ i 1))) ((= i 9) (vector-set! v 4 *hero*))
      (vector-set! v i (generate-card)))
    v))
