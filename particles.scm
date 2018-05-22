;; simple particle engine

;; uses widgets (*gasp*)

;; need to have a pool of sprite widgets that will be used by the
;; particles as they are created?

;; requires 2D vector (as in physics) support (or does it...)
(include "vec2.scm")

;; Instructions
;; create a particle engine (you don't need more than one)
;; create a particle source
;; add the source to the engine and it will create particles by itself

;; deleting sources?
;; ephemeral sources? (have a ttl or other kind of dying condition)

(define <particle>
  (make-class (list <object>)
              '(texture position velocity angle angular-velocity color size ttl sprite)))

(define <particle-source>
  (make-class (list <object>)
              '(rng location maxcount particles textures update-fn generate-fn)))

(define <particle-engine>
  (make-class (list <object>)
              '(sources)))

(define-method initialize (<particle>) (obj args)
  (slot-set! obj 'texture (nth 0 args))
  (slot-set! obj 'position (nth 1 args))
  (slot-set! obj 'velocity (nth 2 args))
  (slot-set! obj 'angle (nth 3 args))
  (slot-set! obj 'angular-velocity (nth 4 args))
  (slot-set! obj 'color (nth 5 args))
  (slot-set! obj 'size (nth 6 args))
  (slot-set! obj 'ttl (nth 7 args))
  (slot-set! obj 'sprite (glgui-sprite gui-particles 'image (nth 0 args)))
  )

(define-method initialize (<particle-source>) (obj args)
  (slot-set! obj 'location (nth 0 args))
  (slot-set! obj 'maxcount 10)
  (slot-set! obj 'update-fn (nth 1 args))
  (slot-set! obj 'generate-fn (nth 2 args))
  (slot-set! obj 'particles (make-vector 10 #f)))

(define-method initialize (<particle-engine>) (obj args)
  (slot-set! obj 'sources (list)))


(define (make-particle1 x y vx vy)
  (make <particle> diamond.img (make <vec2> x y) (make <vec2> vx vy) 0 -5 0 0 50))

(define (recycle-particle1 obj x y vx vy)
  (slot-set! (<- obj position) 'x x)
  (slot-set! (<- obj position) 'y y)
  (slot-set! (<- obj velocity) 'x vx)
  (slot-set! (<- obj velocity) 'y vy)
  (slot-set! obj 'angle 0)
  (slot-set! obj 'ttl 50)
  obj)

(define (make-particle-source x y update-fn generate-fn)
  (make <particle-source> (make <vec2> x y) update-fn generate-fn))

(define (simple-update-particle obj)
  (slot-set! obj 'ttl (- (slot-ref obj 'ttl) 1))
  (slot-set! obj 'position (v+= (slot-ref obj 'position) (slot-ref obj 'velocity)))
  (slot-set! obj 'angle (+ (slot-ref obj 'angle) (slot-ref obj 'angular-velocity))))

;; the generic function is defined in cells
;; TODO namespace this or something...
;; (define update (make-generic))

;; calling this method makes thing about 8x slower, as noted below
(define-method update (<particle> <procedure>) (obj fn)
  (fn obj))

(define-method update (<particle-source>) (obj)
  ;; remove dead particles
  ;; add new particles
  (let ((parts (<- obj particles)))
    (do ((i 0 (+ i 1))) ((= i (vector-length parts)))
      (cond ((not (vector-ref parts i))
             ;;(display (string-append "GENERATING #" (number->string i))) (newline)
             (let ((locx (+ (<- obj location x) (* 10 (- (random-integer 5) 2))))
                   (locy (+ (<- obj location y) (* 10 (- (random-integer 5) 2))))
                   (vx (- (random-integer 21) 10))
                   (vy (- (random-integer 31) 15)))
               (vector-set! parts i ((<- obj generate-fn) locx locy vx vy))))
            ((<= (<- (vector-ref parts i) ttl) 0)
             ;;(display (string-append "RECYCLING #" (number->string i))) (newline)
             (let ((locx (+ (<- obj location x) (* 10 (- (random-integer 5) 2))))
                   (locy (+ (<- obj location y) (* 10 (- (random-integer 5) 2))))
                   (vx (- (random-integer 21) 10))
                   (vy (- (random-integer 31) 15)))
               (recycle-particle1 (vector-ref parts i) locx locy vx vy))))
      ;; about 8x performance improvement!
      ;;(update (vector-ref parts i) (<- obj update-fn))
      ((<- obj update-fn) (vector-ref parts i)))
    
    ;;(display (string-append "Particle count: " (number->string (vector-length parts)))) (newline)
    ))

(define-method update (<particle-engine>) (obj)
  (for-each update (<- obj sources)))

(define add-source (make-generic))
(define-method add-source (<particle-engine> <particle-source>) (obj source)
  (slot-set! obj 'sources (cons source (<- obj sources))))

(define destroy (make-generic))
(define-method destroy (<particle-source>) (obj)
  (slot-set! obj 'particles
             (vector-map (lambda (x)
                           (glgui-widget-delete gui-particles (<- x sprite))
                           #f)
                         (<- obj particles))))

(define remove-source (make-generic))
(define-method remove-source (<particle-engine> <particle-source>) (obj source)
  ;; destroy the particles
  ;; or... allow them to finish? maybe the particle engine should
  ;; manage the particles that the sources produce
  (destroy source)
  (slot-set! obj 'sources (list-delete-item (<- obj sources) source)))

(define draw (make-generic))
(define-method draw (<particle>) (obj)
  ;; (glgui-widget-set! gui-particles (<- obj sprite) 'x (<- obj position x))
  ;; (glgui-widget-set! gui-particles (<- obj sprite) 'y (<- obj position y))
  ;; (glgui-widget-set! gui-particles (<- obj sprite) 'angle (<- obj angle))
  (table-set! (<- obj sprite) 'x (<- obj position x))
  (table-set! (<- obj sprite) 'y (<- obj position y))
  (table-set! (<- obj sprite) 'angle (<- obj angle)))

(define-method draw (<particle-source>) (obj)
  (let ((parts (<- obj particles)))
    (do ((i 0 (+ i 1))) ((= i (vector-length parts)))
      (draw (vector-ref parts i)))))

(define-method draw (<particle-engine>) (obj)
  (for-each draw (<- obj sources)))

