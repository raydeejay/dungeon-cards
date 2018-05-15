(define <card> (make-class (list <object>) '(widget-class image)))
(add-method initialize
            (make-method (list <card>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'widget-class <cell>))))

(define <hero> (make-class (list <card>) '(hp maxhp coins weapon)))
(add-method initialize
            (make-method (list <hero>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'widget-class <hero-cell>)
                           (slot-set! obj 'image hero.img)
                           (slot-set! obj 'maxhp 10)
                           (slot-set! obj 'hp 10)
                           (slot-set! obj 'coins 0)
                           (slot-set! obj 'weapon #f))))


(define <treasure> (make-class (list <card>) '(value)))

(define <coin> (make-class (list <treasure>) '()))
(add-method initialize
            (make-method (list <coin>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'widget-class <item-cell>)
                           (slot-set! obj 'image coin.img)
                           (slot-set! obj 'value (+ 1 (random-integer 9))))))


(define <potion> (make-class (list <card>) '(value)))
(add-method initialize
            (make-method (list <potion>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'widget-class <item-cell>)
                           (slot-set! obj 'image potion.img)
                           (slot-set! obj 'value (+ 1 (random-integer 9))))))


(define <chest> (make-class (list <card>) '()))
(add-method initialize
            (make-method (list <chest>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'image chest.img))))


(define <weapon> (make-class (list <card>) '(damage)))
(add-method initialize
            (make-method (list <weapon>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'widget-class <weapon-cell>)
                           (slot-set! obj 'image weapon.img)
                           (slot-set! obj 'damage (+ 1 (random-integer 9))))))


(define <enemy> (make-class (list <card>) '(hp maxhp)))
(add-method initialize
            (make-method (list <enemy>)
                         (lambda (call-next-method obj args)
                           (call-next-method)
                           (slot-set! obj 'widget-class <living-cell>)
                           (slot-set! obj 'hp (slot-ref obj 'maxhp)))))

(define <demon> (make-class (list <enemy>) '()))
(add-method initialize
            (make-method (list <demon>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'image demon.img)
                           (slot-set! obj 'maxhp (+ 1 (random-integer 8)))
                           (call-next-method))))

(define <skeleton> (make-class (list <enemy>) '()))
(add-method initialize
            (make-method (list <skeleton>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'image skeleton.img)
                           (slot-set! obj 'maxhp (+ 1 (random-integer 4)))
                           (call-next-method))))

(define <goblin> (make-class (list <enemy>) '()))
(add-method initialize
            (make-method (list <goblin>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'image goblin.img)
                           (slot-set! obj 'maxhp (+ 1 (random-integer 6)))
                           (call-next-method))))

(define <slime> (make-class (list <enemy>) '()))
(add-method initialize
            (make-method (list <slime>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'image slime.img)
                           (slot-set! obj 'maxhp (+ 1 (random-integer 5)))
                           (call-next-method))))

(define <rat> (make-class (list <enemy>) '()))
(add-method initialize
            (make-method (list <rat>)
                         (lambda (call-next-method obj args)
                           (slot-set! obj 'image rat.img)
                           (slot-set! obj 'maxhp (+ 1 (random-integer 3)))
                           (call-next-method))))

(define <weapon> (make-class (list <card>) '(damage)))


;; return T if the entity should be buried
(define interact (make-generic))

(add-method interact
            (make-method (list <hero> <coin>)
                         (lambda (call-next-method hero coin)
                           (slot-set! hero 'coins (+ (slot-ref hero 'coins)
                                                     (slot-ref coin 'value)))
                           (audiofile-play *coinfx*)
                           #t)))

(add-method interact
            (make-method (list <hero> <potion>)
                         (lambda (call-next-method hero potion)
                           (audiofile-play *potionfx*)
                           (slot-set! hero 'hp (+ (slot-ref hero 'hp)
                                                  (slot-ref potion 'value)))
                           #t)))

(add-method interact
            (make-method (list <hero> <chest>)
                         (lambda (call-next-method hero chest)
                           (audiofile-play *chestfx*)
                           'open-chest)))

(add-method interact
            (make-method (list <hero> <weapon>)
                         (lambda (call-next-method hero weapon)
                           (audiofile-play *swordfx*)
                           (slot-set! hero 'weapon weapon)
                           #t)))

(add-method interact
            (make-method (list <hero> <enemy>)
                         (lambda (call-next-method hero enemy)
                           (audiofile-play *punchfx*)
                           (let ((wep (slot-ref hero 'weapon)))
                             (if wep
                                 (interact wep enemy)
                                 (begin (slot-set! hero 'hp (- (slot-ref hero 'hp)
                                                               (slot-ref enemy 'hp)))
                                        #t))))))

(add-method interact
            (make-method (list <weapon> <enemy>)
                         (lambda (call-next-method weapon enemy)
                           (cond ((> (slot-ref enemy 'hp) (slot-ref weapon 'damage))
                                  (slot-set! enemy 'hp (- (slot-ref enemy 'hp)
                                                          (slot-ref weapon 'damage)))
                                  (slot-set! *hero* 'weapon #f)
                                  #f)
                                 ((= (slot-ref enemy 'hp) (slot-ref weapon 'damage))
                                  (slot-set! *hero* 'weapon #f)
                                  #t)
                                 ((< (slot-ref enemy 'hp) (slot-ref weapon 'damage))
                                  (slot-set! weapon 'damage (- (slot-ref weapon 'damage)
                                                               (slot-ref enemy 'hp)))
                                  #t)))))

(define (generate-card)
  (let ((kind (random-elt (list
                     <coin> <coin> <coin> <coin>
                     <skeleton> <skeleton> <skeleton>
                     <goblin> <goblin> <goblin>
                     <demon>
                     <potion>
                     <chest>
                     <weapon> <weapon>
                     ))))
    (make kind)))
