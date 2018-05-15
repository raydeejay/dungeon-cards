;; --------------------
;; game-over state
(define restart-button #f)

(define (game-over-draw)
  (let* ((w (glgui-width-get))
         (h (glgui-height-get)))
    ;; game-over screen
    ;;  title
    ;;  restart button
    (set! restart-button (glgui-button-string gui 200 360 100 20 "START" calc_14.fnt
                                            (lambda args
                                              (game-over-end)
                                              (game-draw)
                                              (set! handle-fn game-handle))))))

(define (game-over-handle t x y)
  (let ((skipevent #f))
    (if (= t EVENT_KEYPRESS)
        (cond ((= x EVENT_KEYBACK)          (terminate))
              ((= x EVENT_KEYESCAPE)     (terminate))))
    (if (not skipevent) (glgui-event gui t x y))))

(define (game-over-end)
  (glgui-widget-delete gui restart-button))
