(define dungeon-name #f)
(define back-button #f)
(define power-1-ui #f)
(define power-2-ui #f)
(define power-3-ui #f)
(define coins-ui #f)
(define cells (make-vector 9))
(define hero-cell 4)
(define *ticker* #f)

(define (game-handle t x y)
  ;;(tick-fn)
  ;; (update-gui)
  (tick *ticker*)
  (update *particle-engine*)
  (draw *particle-engine*)
  (let ((skipevent #f))
    (if (= t EVENT_KEYPRESS)
        (cond ((= x EVENT_KEYBACK)       (terminate))
              ((= x EVENT_KEYESCAPE)     (terminate))))
    ;; (if (= t EVENT_KEYRELEASE)
    ;;     (cond ((= x (char->integer #\q)) (set! skipevent #t))))
    (if (not skipevent) (glgui-event gui t x y))))

(define (game-draw)
  (let* ((w (glgui-width-get))
         (h (glgui-height-get)))
    (set! *hero* (make <hero>))
    (set! *field* (generate-field))
    (set! *ticker* (make <ticker>))

    ;; dungeon screen
    ;; power1 power2 power3  ----- coins (multiplier)
    ;; (set! hp-ui (glgui-label gui 0 (- h 20) 110 16
    ;;                             (number->string (slot-ref *hero* 'hp)) calc_14.fnt White))
    (set! power-1-ui (glgui-image gui 4 (- h 20) 16 16 heart.img White))
    (set! power-2-ui (glgui-image gui 24 (- h 20) 16 16 smallcoin.img White))
    (set! power-3-ui (glgui-image gui 44 (- h 20) 16 16 smallcoin.img White))
    (set! coins-ui (glgui-label gui (- w 120) (- h 20) 110 16
                                (number->string (slot-ref *hero* 'coins)) calc_14.fnt White))
    (glgui-widget-set! gui coins-ui 'align GUI_ALIGNRIGHT)


    ;; particle layer on top
    (set! gui-canvas (glgui-container gui 0 0 w h))
    (set! gui-particles (glgui-container gui 0 0 w h))
    (set! *particle-engine* (make <particle-engine>))

    ;; cells
    (do ((i 0 (+ i 1))) ((= i 9) (set! hero-cell 4))
      (let ((class (slot-ref (vector-ref *field* i) 'widget-class)))
        (vector-set! cells i (make class gui-canvas i))))
    (update-gui)

    ;; back button ------- dungeon name
    (set! back-button (glgui-button-string gui 0 0 60 40 "<<" calc_14.fnt
                                           (lambda args
                                             (game-end)
                                             (menu-draw)
                                             (set! handle-fn menu-handle))))
    (set! dungeon-name (glgui-label gui (- w 200) 8 190 16 "Dungeon 1" calc_24.fnt White))
    (glgui-widget-set! gui dungeon-name 'align GUI_ALIGNRIGHT)))

(define (game-end)
  (for-each (lambda (x) (glgui-widget-delete gui x))
            (table-ref gui 'widget-list)))
