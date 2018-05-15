;; Button functions
;; ------------------
(define (lerp a b t) (+ (* (- 1 t) a) (* t b)))

(define xa 0)
(define xb 50)

(define ticks 0)

(define (tick-fn)
  (let ((distance (- xb xa)))
    ;; (glgui-widget-set! gui keypad 'x
    ;;                    (if (>= (modulo ticks (* 2 distance)) distance)
    ;;                        (lerp xb xa (/ (modulo ticks distance) distance))
    ;;                        (lerp xa xb (/ (modulo ticks distance) distance))))
    #t)
  (set! ticks (+ 1 ticks)))
