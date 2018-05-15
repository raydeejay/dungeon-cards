#|
MIT License
|#


;; A simple game of roaming around dungeons and collecting
;; treasure. And not dying. That is rather important.

;; by raydeejay

;; cards take 3*120 width, 3*160 height
(define *width* 400)
(define *height* 600)

;; include files
(include "utils.scm")
(include "menu.scm")
(include "cards.scm")
(include "cells.scm")
(include "field.scm")
(include "game.scm")
(include "game-over.scm")
(include "anim.scm")

;; global variables
(define gui #f)

;; audio
(audiofile-init)
(define *coinfx* (audiofile-load "coin"))
(define *punchfx* (audiofile-load "punch"))
(define *potionfx* (audiofile-load "potion"))
(define *swordfx* (audiofile-load "sword"))
(define *chestfx* (audiofile-load "chest"))

;; --------------------
;; entry point

;;(define *db* #f)
(define handle-fn menu-handle)

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
