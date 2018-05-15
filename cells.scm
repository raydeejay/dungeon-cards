;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CELLS

(define <cell> (make-class (list <object>) '(card container image)))
(add-method
 initialize
 (make-method
  (list <cell>)
  (lambda (call-next-method cell args)
    (let* ((gui (car args))
           (i (cadr args))
           (card (vector-ref *field* i)))
      (let* ((cont (glgui-container gui (slot->x i) (slot->y i) 120 160))
             (image (glgui-image cont 0 0 120 160 (slot-ref card 'image) White)))
        (slot-set! cell 'container cont)
        (slot-set! cell 'card card)
        (slot-set! cell 'image image)
        (glgui-widget-set! cont image 'callback (make-callback cell)))))))

(define <living-cell> (make-class (list <cell>) '(hp)))
(add-method
 initialize
 (make-method
  (list <living-cell>)
  (lambda (call-next-method cell args)
    (call-next-method)
    (glgui-image (slot-ref cell 'container) 100 140 20 20 heart.img White)
    (slot-set! cell 'hp (glgui-label (slot-ref cell 'container) 40 140 60 20 "-" calc_14.fnt White))
    (glgui-widget-set! (slot-ref cell 'container) (slot-ref cell 'hp) 'align GUI_ALIGNRIGHT))))

(define <item-cell> (make-class (list <cell>) '(value)))
(add-method
 initialize
 (make-method
  (list <item-cell>)
  (lambda (call-next-method cell args)
    (call-next-method)
    (slot-set! cell 'value (glgui-label (slot-ref cell 'container) 100 0 20 20 "-" calc_14.fnt White)))))

(define <weapon-cell> (make-class (list <cell>) '(damage)))
(add-method
 initialize
 (make-method
  (list <weapon-cell>)
  (lambda (call-next-method cell args)
    (call-next-method)
    (slot-set! cell 'damage (glgui-label (slot-ref cell 'container) 100 0 20 20 "-" calc_14.fnt White)))))

(define <hero-cell> (make-class (list <living-cell>) '(weapon)))
(add-method
 initialize
 (make-method
  (list <hero-cell>)
  (lambda (call-next-method cell args)
    (call-next-method)
    (slot-set! cell 'weapon (glgui-label (slot-ref cell 'container) 0 0 20 20 "-" calc_14.fnt White)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPUTE FIELD SLOT
(define (compute-slot cell)
  (xy->slot (table-ref (slot-ref cell 'container) 'xofs)
            (table-ref (slot-ref cell 'container) 'yofs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CALLBACK
(define make-callback (make-generic))

(add-method
 make-callback
 (make-method
  (list <cell>)
  (lambda (call-next-method cell)
    (lambda args
      ;; (pretty-print (table->list (car args))) (newline)
      ;; (pretty-print (table->list (cadr args))) (newline)
      (let ((from hero-cell)
            (to (compute-slot cell)))
        (if (can-move from to)
            (let ((action (interact (vector-ref *field* from) (vector-ref *field* to))))
              (case action
                ((open-chest)
                 (vector-set! *field* to (make <coin>))
                 (glgui-widget-delete gui (slot-ref (vector-ref cells to) 'container))
                 (vector-set! cells to (make (slot-ref (vector-ref *field* to) 'widget-class) gui to))
                 (update-gui))
                ((#t)
                 (add-to-ticker *ticker* 10
                                (lambda (t)
                                  (glgui-widget-set! gui
                                                     (slot-ref (vector-ref cells from) 'container)
                                                     'xofs
                                                     (lerp (slot->x from) (slot->x to) t))
                                  (glgui-widget-set! gui
                                                     (slot-ref (vector-ref cells from) 'container)
                                                     'yofs
                                                     (lerp (slot->y from) (slot->y to) t)))
                                (lambda ()
                                  (move-to-target from to)
                                  (glgui-widget-delete gui (slot-ref (vector-ref cells from) 'container))
                                  (vector-set! cells from (make (slot-ref (vector-ref *field* from) 'widget-class) gui from))
                                  (glgui-widget-delete gui (slot-ref (vector-ref cells to) 'container))
                                  (vector-set! cells to (make (slot-ref (vector-ref *field* to) 'widget-class) gui to))
                                  (update-gui)
                                  #t)))
                ((#f) (update-gui))))))

      ;; update all the widgets
      ;; or it can also be done in the handle function!
      ;;(update-gui)

      ;; check for death
      (if (< (slot-ref *hero* 'hp) 1)
          (begin (let ((m (settings-ref "money")))
                   ;; apparently one can't rely on this operation completing immediately (?)...
                   ;; (lmdb-set! *db* 'money (+ (slot-ref *hero* 'coins) m))
                   (settings-set! "money" (+ (slot-ref *hero* 'coins) m)))
                 (game-end)
                 (game-over-draw)
                 (set! handle-fn game-over-handle)))
      #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATE CELLS
(define update (make-generic))

(add-method
 update
 (make-method
  (list <cell>)
  (lambda (call-next-method cell)
    (let* ((cont (slot-ref cell 'container))
           (x (glgui-widget-get gui cont 'xofs))
           (y (glgui-widget-get gui cont 'yofs)))
      (slot-set! cell 'card (vector-ref *field* (xy->slot x y)))
      (let ((card (slot-ref cell 'card))
            (image (slot-ref cell 'image)))
        (glgui-widget-set! cont image 'image (slot-ref card 'image))))
    cell)))

(add-method
 update
 (make-method
  (list <living-cell>)
  (lambda (call-next-method cell)
    (call-next-method)
    (let ((cont (slot-ref cell 'container))
          (hp (slot-ref cell 'hp))
          (card (slot-ref cell 'card)))
      (glgui-widget-set! cont hp 'label (number->string (slot-ref card 'hp))))
    cell)))

(add-method
 update
 (make-method
  (list <item-cell>)
  (lambda (call-next-method cell)
    (call-next-method)
    (let ((cont (slot-ref cell 'container))
          (value (slot-ref cell 'value))
          (card (slot-ref cell 'card)))
      (glgui-widget-set! cont value 'label (number->string (slot-ref card 'value))))
    cell)))

(add-method
 update
 (make-method
  (list <weapon-cell>)
  (lambda (call-next-method cell)
    (call-next-method)
    (let ((cont (slot-ref cell 'container))
          (damage (slot-ref cell 'damage))
          (card (slot-ref cell 'card)))
      (glgui-widget-set! cont damage 'label (number->string (slot-ref card 'damage))))
    cell)))

(add-method
 update
 (make-method
  (list <hero-cell>)
  (lambda (call-next-method cell)
    (call-next-method)
    (let* ((cont (slot-ref cell 'container))
           (weapon (slot-ref cell 'weapon))
           (card (slot-ref cell 'card))
           (hp (slot-ref cell 'hp))
           (w (slot-ref card 'weapon)))
      (glgui-widget-set! cont weapon 'label (if w (number->string (slot-ref w 'damage)) ""))
      (glgui-widget-set! cont hp 'label (string-append (number->string (slot-ref card 'hp))
                                                       "/" (number->string (slot-ref card 'maxhp))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UPDATE GUI
(define (update-gui)
  (do ((i 0 (+ i 1))) ((= i 9))
    (update (vector-ref cells i)))
  (glgui-widget-set! gui coins-ui 'label (number->string (slot-ref *hero* 'coins))))
