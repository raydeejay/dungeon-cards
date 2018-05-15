(define <ticker> (make-class (list <object>) '(elements)))
(define <ticker-element> (make-class (list <object>) '(dead count max fn end-fn)))

(add-method
 initialize
 (make-method
  (list <ticker>)
  (lambda (call-next-method obj args)
    (slot-set! obj 'elements (list)))))

(add-method
 initialize
 (make-method
  (list <ticker-element>)
  (lambda (call-next-method obj args)
    (slot-set! obj 'dead #f)
    (slot-set! obj 'count 0)
    (slot-set! obj 'max (car args))
    (slot-set! obj 'fn (cadr args))
    (slot-set! obj 'end-fn (caddr args))
    )))

(define tick (make-generic))
(add-method
 tick
 (make-method
  (list <ticker>)
  (lambda (call-next-method ticker)
    (for-each (lambda (x) (tick x)) (slot-ref ticker 'elements))
    (purge-ticker ticker))))

(add-method
 tick
 (make-method
  (list <ticker-element>)
  (lambda (call-next-method obj)
        (slot-set! obj 'count (+ 1 (slot-ref obj 'count)))
    (if (> (slot-ref obj 'count) (slot-ref obj 'max))
        (begin (slot-set! obj 'dead #t)
               ((slot-ref obj 'end-fn)))
        ((slot-ref obj 'fn) (/ (slot-ref obj 'count) (slot-ref obj 'max)))))))

(define (add-to-ticker ticker ticks fn end-fn)
  (slot-set! ticker 'elements
             (append (slot-ref ticker 'elements)
                     (list (make <ticker-element> ticks fn end-fn)))))

(define (purge-ticker ticker)
  (slot-set! ticker 'elements
             (list-keep (slot-ref ticker 'elements) (lambda (x) (not (slot-ref x 'dead))))))
