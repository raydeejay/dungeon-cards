;; --------------------
;; game state

(define *hero* (make <hero>))

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

(define *field* (make-vector 9))

(define (can-move from to)
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
