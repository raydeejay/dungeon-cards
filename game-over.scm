;; --------------------
;; game-over state
(define restart-button #f)
(define death-text #f)

(define (game-over-draw)
  (let* ((w (glgui-width-get))
         (h (glgui-height-get)))
    (set! death-text (glgui-label gui 200 400 100 20 (number->string (slot-ref *hero* 'coins)) calc_14.fnt White))
    (set! restart-button (glgui-button-string gui 140 240 120 40 "OK" calc_24.fnt
                                              (lambda args
                                                (game-over-end)
                                                (menu-draw)
                                                (set! handle-fn menu-handle))))))

(define (game-over-handle t x y)
  (let ((skipevent #f))
    (if (= t EVENT_KEYPRESS)
        (cond ((= x EVENT_KEYBACK)          (terminate))
              ((= x EVENT_KEYESCAPE)     (terminate))))
    (if (not skipevent) (glgui-event gui t x y))))

(define (game-over-end)
  (glgui-widget-delete gui restart-button)
  (glgui-widget-delete gui death-text))
