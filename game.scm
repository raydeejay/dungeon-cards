(define (game-handle t x y)
  ;;(tick-fn)
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

    ;; dungeon screen
    ;; power1 power2 power3  ----- coins (multiplier)
    ;; (set! hp-ui (glgui-label gui 0 (- h 20) 110 16
    ;;                             (number->string (slot-ref *hero* 'hp)) calc_14.fnt White))
    (set! coins-ui (glgui-label gui (- w 120) (- h 20) 110 16
                                (number->string (slot-ref *hero* 'coins)) calc_14.fnt White))
    (glgui-widget-set! gui coins-ui 'align GUI_ALIGNRIGHT)


    ;; cells
    (do ((i 0 (+ i 1))) ((= i 9) (set! hero-cell 4))
      (let ((class (slot-ref (vector-ref *field* i) 'widget-class)))
        (vector-set! cells i (make class gui i))))
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
  (do ((i 0 (+ i 1))) ((= i 9))
    (glgui-widget-delete gui (slot-ref (vector-ref cells i) 'container)))
  (glgui-widget-delete gui dungeon-name)
  (glgui-widget-delete gui back-button)
  (glgui-widget-delete gui coins-ui))
