(define (find item seq)
  (let ((x (member item seq)))
    (if x (cadr x) '())))

(define (random-elt l) (list-ref l (random-integer (length l))))
