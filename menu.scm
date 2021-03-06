;; --------------------
;; menu state
(define start-button #f)
(define title #f)

(define (menu-draw)
  (let* ((w (glgui-width-get))
         (h (glgui-height-get)))
    (set! coins-ui (glgui-label gui 5 (- h 20) 110 16
                                (number->string (settings-ref "money")) calc_14.fnt White))
    (set! title (glgui-image gui 20 360 360 120 title.img White))
    (set! start-button (glgui-button-string gui 120 200 160 40 "START" calc_24.fnt
                                            (lambda args
                                              (menu-end)
                                              (game-draw)
                                              (set! handle-fn game-handle))))))

(define (menu-handle t x y)
  (let ((skipevent #f))
    (if (= t EVENT_KEYPRESS)
        (cond ((= x EVENT_KEYBACK)       (terminate))
              ((= x EVENT_KEYESCAPE)     (terminate))))
    (if (not skipevent) (glgui-event gui t x y))))

(define (menu-end)
  (for-each (lambda (x) (glgui-widget-delete gui x))
            (table-ref gui 'widget-list)))
