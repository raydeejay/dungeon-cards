;; TODO move these things to their proper place
;; (##namespace ("vec2#"))
;; (##include "~~lib/gambit#.scm")

;;(namespace ("vec2" <vec2> v+ v- v* v/ vset! vrotate vnormalize vdist vlength vtruncate vortho vdot vcross))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vec2.scm
;;
;; a simple tinyclos library to work with 2D vectors

(define <vec2> (make-class (list <object>) '(x y)))

(add-method
 initialize
 (make-method
  (list <vec2>)
  (lambda (call-next-method obj args)
    (slot-set! obj 'x (car args))
    (slot-set! obj 'y (cadr args)))))


;; TODO ideally, wrap the original math operators around generic functions
;; TODO figure out how to do the thing below without breaking the system

;; (let ((++ +))
;;   (set! + (make-generic))
;;   (add-method + (make-method (list <number>)
;;                              (lambda (cnm . args) (apply ++ args)))))

;; (add-method + (make-method (list <vec2>)
;;                            (lambda (cnm a b)
;;                              (make <vec2>
;;                                (+ (<- a x) (<- b x))
;;                                (+ (<- a y) (<- b y))))))


(define v+ (make-generic))

(define-method v+ (<vec2> <number>) (v s)
  (make <vec2> (+ (<- v x) s) (+ (<- v y) s)))

(define-method v+ (<vec2> <vec2>) (v w)
  (make <vec2> (+ (<- v x) (<- w x)) (+ (<- v y) (<- w y))))


(define v+= (make-generic))

(define-method v+= (<vec2> <number>) (v s)
  (slot-set v 'x (+ (<- v x) s))
  (slot-set v 'y (+ (<- v y) s))
  v)

(define-method v+= (<vec2> <vec2>) (v w)
  (slot-set! v 'x (+ (<- v x) (<- w x)))
  (slot-set! v 'y (+ (<- v y) (<- w y)))
  v)


(define v- (make-generic))

(define-method v- (<vec2> <number>) (v s)
  (make <vec2> (- (<- v x) s) (- (<- v y) s)))

(define-method v- (<vec2> <vec2>) (v w)
  (make <vec2> (- (<- v x) (<- w x)) (- (<- v y) (<- w y))))


(define v* (make-generic))

(define-method v* (<vec2> <number>) (v s)
  (make <vec2> (* (<- v x) s) (* (<- v y) s)))


(define v/ (make-generic))

(define-method v/ (<vec2> <number>) (v s)
  (make <vec2> (/ (<- v x) s) (/ (<- v y) s)))


(define vset! (make-generic))

(define-method vset! (<vec2> <number> <number>) (v x y)
  (slot-set! v 'x x)
  (slot-set! v 'y y))


(define vrotate (make-generic))

(define-method vrotate (<vec2> <number>) (v deg)
  (let* ((pi 3.141592)
         (theta (* pi (/ deg 180.0)))
         (c (cos theta))
         (s (sin theta))
         (x (- (* (<- v x) c) (* (<- v y) s)))
         (y (+ (* (<- v x) x) (* (<- v y) c))))
    (slot-set! v 'x x)
    (slot-set! v 'y y)))


(define vnormalize (make-generic))

(define-method vnormalize (<vec2>) (v)
  (let ((vl (vlength v)))
    (if (zero? vl)
        v
        (v/ v vl))))


(define vdist (make-generic))

(define-method vdist (<vec2> <vec2>) (v w)
  (vlength (make <vec2> (- (<- w x) (<- v x)) (- (<- w y) (<- v y)))))


(define vlength (make-generic))

(define-method vlength (<vec2>) (v)
  (let ((x (<- v x))
        (y (<- v y)))
    (sqrt (+ (* x x) (* y y)))))


(define vtruncate (make-generic))

(define-method vtruncate (<vec2> <number>) (v len)
  (let* ((x (<- v x))
         (y (<- v y))
         (angle (atan y x)))
    (slot-set! v 'x (* len (cos angle)))
    (slot-set! v 'y (* len (sin angle)))))


(define vortho (make-generic))

(define-method vortho (<vec2>) (v)
  (make <vec2> (<- v y) (- (<- v x))))


(define vdot (make-generic))

(define-method vdot (<vec2> <vec2>) (v w)
  (+ (* (<- v x) (<- w x)) (* (<- v y) (<- w y))))


(define vcross (make-generic))

(define-method vcross (<vec2> <vec2>) (v w)
  (- (* (<- v x) (<- w y)) (* (<- v y) (<- w x))))
