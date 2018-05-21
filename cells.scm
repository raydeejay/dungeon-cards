;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CELLS

(define <cell> (make-class (list <object>) '(card container image)))

(define-method initialize (<cell>) (cell args)
  (let* ((gui (car args))
         (i (cadr args))
         (card (vector-ref *field* i)))
    (let* ((cont (glgui-container gui (slot->x i) (slot->y i) 120 160))
           (image (glgui-image cont 0 0 120 160 (slot-ref card 'image) White)))
      (slot-set! cell 'container cont)
      (slot-set! cell 'card card)
      (slot-set! cell 'image image)
      (glgui-widget-set! cont image 'callback (make-callback cell)))))

(define <living-cell> (make-class (list <cell>) '(hp)))

(define-method initialize (<living-cell>) (cell args)
  (call-next-method)
  (glgui-image (slot-ref cell 'container) 100 140 20 20 heart.img White)
  (slot-set! cell 'hp (glgui-label (slot-ref cell 'container) 40 140 60 20 "-" calc_14.fnt White))
  (glgui-widget-set! (slot-ref cell 'container) (slot-ref cell 'hp) 'align GUI_ALIGNRIGHT))

(define <item-cell> (make-class (list <cell>) '(value)))

(define-method initialize (<item-cell>) (cell args)
  (call-next-method)
  (slot-set! cell 'value (glgui-label (slot-ref cell 'container) 100 0 20 20 "-" calc_14.fnt White)) )

(define <weapon-cell> (make-class (list <cell>) '(damage)))

(define-method initialize (<weapon-cell>) (cell args)
  (call-next-method)
  (slot-set! cell 'damage (glgui-label (slot-ref cell 'container) 100 0 20 20 "-" calc_14.fnt White)))

(define <hero-cell> (make-class (list <living-cell>) '(weapon)))

(define-method initialize (<hero-cell>) (cell args)
  (call-next-method)
  (slot-set! cell 'weapon (glgui-label (slot-ref cell 'container) 0 0 20 20 "-" calc_14.fnt White)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPUTE FIELD SLOT
(define (compute-slot cell)
  (xy->slot (table-ref (slot-ref cell 'container) 'xofs)
            (table-ref (slot-ref cell 'container) 'yofs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK
(define make-callback (make-generic))

(define (open-chest pos)
  (vector-set! *field* pos (make <coin>))
  (glgui-widget-delete gui-canvas (slot-ref (vector-ref cells pos) 'container))
  (vector-set! cells pos (make (slot-ref (vector-ref *field* pos) 'widget-class) gui-canvas pos)))

(define (move-to-front w)
  ;; move the widget to the back of the gui's widget-list to ensure it's drawn on top of anything else
  (let ((lista (table-ref gui-canvas 'widget-list)))
    (table-set! gui-canvas 'widget-list
                (let ((h (list-keep lista
                                    (lambda (x)
                                      (equal? x w)))))
                  (append (list-delete-item lista (car h)) h)))))

(define (kill-cell from to)
  ;; move the widget to the back of the gui's widget-list to ensure it's drawn on top of anything else
  (move-to-front (slot-ref (vector-ref cells from) 'container))
  ;; reduce the size of the target card
  (add-to-ticker *ticker* 10
                 (lambda (t d)
                   (let ((cell-to (slot-ref (vector-ref cells to) 'container)))
                     (glgui-widget-set! gui-canvas cell-to 'w (lerp 120 0 t d))
                     (glgui-widget-set! gui-canvas cell-to 'h (lerp 160 0 t d))
                     (glgui-widget-set! gui-canvas cell-to 'xofs (easeinout-quad (slot->x to) (+ 60 (slot->x to)) t d))
                     (glgui-widget-set! gui-canvas cell-to 'yofs (easeinout-quad (slot->y to) (+ 80 (slot->y to)) t d))))
                 (lambda () #t))
  ;; move the hero card to its new slot
  (add-to-ticker *ticker* 100
                 (lambda (t d)
                   (let ((cell-from (slot-ref (vector-ref cells from) 'container)))
                     (glgui-widget-set! gui-canvas cell-from 'xofs (easeinout-quad (slot->x from) (slot->x to) t d))
                     (glgui-widget-set! gui-canvas cell-from 'yofs (easeinout-quad (slot->y from) (slot->y to) t d))))
                 (lambda ()
                   (move-to-target from to)
                   ;; remove the old widgets and make
                   ;; new ones, because each card
                   ;; requires its own kind of widget
                   (glgui-widget-delete gui-canvas (slot-ref (vector-ref cells from) 'container))
                   (vector-set! cells from
                                (make (slot-ref (vector-ref *field* from) 'widget-class) gui-canvas from))
                   (glgui-widget-delete gui-canvas (slot-ref (vector-ref cells to) 'container))
                   (vector-set! cells to
                                (make (slot-ref (vector-ref *field* to) 'widget-class) gui-canvas to))
                   ;; update the stats displays and such
                   (update-gui)))
  ;; add some particles
  (let ((x0 (+ 60 (slot->x to)))
        (y0 (+ 80 (slot->y to)))
        (targets `((50 . 70) (-50 . 70) (50 . -70) (-50 . -70))))
    (do ((i 0 (+ i 1))) ((= i 4))
      (let ((part (glgui-sprite gui-particles 'x x0 'y x0 'image star.img)))
        (add-to-ticker *ticker* 30
                       (lambda (t d)
                         (glgui-widget-set! gui-particles part 'x (easeinout-quad x0 (+ x0 (car (nth i targets))) t d))
                         (glgui-widget-set! gui-particles part 'y (easeinout-quad y0 (+ y0 (cdr (nth i targets))) t d)))
                       (lambda ()
                         (glgui-widget-delete gui-particles part)))))))

(define-method make-callback (<cell>) (cell)
  (lambda args
    ;; (pretty-print (table->list (car args))) (newline)
    ;; (pretty-print (table->list (cadr args))) (newline)
    (let ((from hero-cell)
          (to (compute-slot cell)))
      (if (can-interact? from to)
          (let ((action (interact (vector-ref *field* from) (vector-ref *field* to))))
            (case action
              ((open-chest) (open-chest to) (update-gui))
              ((#t) (kill-cell from to))
              ((#f) (update-gui))))))
    ;; check for death
    (if (< (slot-ref *hero* 'hp) 1)
        (begin (let ((m (settings-ref "money")))
                 ;; apparently one can't rely on this operation completing immediately (?)...
                 ;; (lmdb-set! *db* 'money (+ (slot-ref *hero* 'coins) m))
                 (settings-set! "money" (+ (slot-ref *hero* 'coins) m)))
               (game-end)
               (game-over-draw)
               (set! handle-fn game-over-handle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATE CELLS
(define update (make-generic))

(define-method update (<cell>) (cell)
  (let* ((cont (slot-ref cell 'container))
         (x (glgui-widget-get gui cont 'xofs))
         (y (glgui-widget-get gui cont 'yofs)))
    (slot-set! cell 'card (vector-ref *field* (xy->slot x y)))
    (let ((card (slot-ref cell 'card))
          (image (slot-ref cell 'image)))
      (glgui-widget-set! cont image 'image (slot-ref card 'image))))
  cell)

(define-method update (<living-cell>) (cell)
  (call-next-method)
  (let ((cont (slot-ref cell 'container))
        (hp (slot-ref cell 'hp))
        (card (slot-ref cell 'card)))
    (glgui-widget-set! cont hp 'label (number->string (slot-ref card 'hp))))
  cell)

(define-method update (<item-cell>) (cell)
  (call-next-method)
  (let ((cont (slot-ref cell 'container))
        (value (slot-ref cell 'value))
        (card (slot-ref cell 'card)))
    (glgui-widget-set! cont value 'label (number->string (slot-ref card 'value))))
  cell)

(define-method update (<weapon-cell>) (cell)
  (call-next-method)
  (let ((cont (slot-ref cell 'container))
        (damage (slot-ref cell 'damage))
        (card (slot-ref cell 'card)))
    (glgui-widget-set! cont damage 'label (number->string (slot-ref card 'damage))))
  cell)

(define-method update (<hero-cell>) (cell)
  (call-next-method)
  (let* ((cont (slot-ref cell 'container))
         (weapon (slot-ref cell 'weapon))
         (card (slot-ref cell 'card))
         (hp (slot-ref cell 'hp))
         (w (slot-ref card 'weapon)))
    (glgui-widget-set! cont weapon 'label (if w (number->string (slot-ref w 'damage)) ""))
    (glgui-widget-set! cont hp 'label (string-append (number->string (slot-ref card 'hp))
                                                     "/" (number->string (slot-ref card 'maxhp))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATE GUI
(define (update-gui)
  (do ((i 0 (+ i 1))) ((= i 9))
    (update (vector-ref cells i)))
  (glgui-widget-set! gui coins-ui 'label (number->string (slot-ref *hero* 'coins))))
