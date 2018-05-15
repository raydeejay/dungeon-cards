#|
MIT License
|#


;; A simple game of roaming around dungeons and collecting
;; treasure. And not dying. That is rather important.

;; by raydeejay

;; cards take 3*120 width, 3*160 height
(define *width* 400)
(define *height* 600)

(include "utils.scm")
(include "menu.scm")
(include "cards.scm")
(include "cells.scm")
(include "field.scm")
(include "game.scm")
(include "game-over.scm")
(include "anim.scm")

(define gui #f)
(define dungeon-name #f)
(define back-button #f)
(define coins-ui #f)
(define cells (make-vector 9))
(define hero-cell 4)

;; update
;; -------------------
;; cards
(define (xy->slot x y)
  (cond ((< (- x 20) 120)
         (cond ((< (- y 60) 160) 0)
               ((< (- y 60) 320) 3)
               ((< (- y 60) 480) 6)
               (#t 0)))
        ((< (- x 20) 240)
         (cond ((< (- y 60) 160) 1)
               ((< (- y 60) 320) 4)
               ((< (- y 60) 480) 7)
               (#t 0)))
        ((< (- x 20) 360)
         (cond ((< (- y 60) 160) 2)
               ((< (- y 60) 320) 5)
               ((< (- y 60) 480) 8)
               (#t 0)))))

(define (slot->x slot)
  (+ 20
     (case slot
       ((0 3 6) 0)
       ((1 4 7) 120)
       ((2 5 8) 240))))

(define (slot->y slot)
  (+ 60
     (case slot
       ((0 1 2) 0)
       ((3 4 5) 160)
       ((6 7 8) 320))))


;; screens
(define handle-fn menu-handle)

;; audio
(audiofile-init)
(define *coinfx* (audiofile-load "coin"))
(define *punchfx* (audiofile-load "punch"))
(define *potionfx* (audiofile-load "potion"))
(define *swordfx* (audiofile-load "sword"))

;; --------------------
;; entry point
;;(define *db* #f)

(main
 ;; initialization
 (lambda (w h)
   (if (not (file-exists? (system-directory)))
       (create-directory (system-directory)))

   (let ((configdirectory (string-append (system-directory) (system-pathseparator) "config")))
     (if (not (file-exists? configdirectory))
         (create-directory configdirectory)))
   (settings-init (list (cons "money" 0)))

   ;; load/init data
   ;; (let ((dbfile (string-append (system-directory) (system-pathseparator) "data.db")))
   ;;   (set! *db* (lmdb-open dbfile)))
   ;; (if (not (lmdb-ref *db* 'money))
   ;;     (lmdb-set! *db* 'money 0))

   ;; start up the app
   (make-window *width* *height*)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))
   (set! handle-fn menu-handle)
   (menu-draw))

 ;; events
 (lambda (t x y) (handle-fn t x y))

 ;; termination
 (lambda () #t)

 ;; suspend
 (lambda () (glgui-suspend))

 ;; resume
 (lambda () (glgui-resume)))

;; eof
