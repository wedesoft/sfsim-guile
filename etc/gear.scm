#!/usr/bin/env guile
!#
; https://www.saylor.org/site/wp-content/uploads/2011/06/MA221-6.1.pdf
(use-modules (glut) (gl) (gl low-level) (glu) (srfi srfi-1) (srfi srfi-26) (ice-9 format))

(define mb 10000)
(define mw 500)
(define b 20)
(define w 220)

(define l 200)
(define k 400.0)
(define d 1500.0)

(define ve 80)
(define loss 0.6)

(define bv 0)
(define wv 0)

(define g 3)

(define fl 440)

(define dt 0.05)

(define main-window #f)


(define (on-idle)
  (define f (- (* k (- l (- w b))) (* d (- wv bv))))
  (set! b (+ b (* bv dt)))
  (set! w (+ w (* wv dt)))
  (set! wv (+ wv (* (+ g (/ f mw)) dt)))
  (set! bv (+ bv (* (- g (/ f mb)) dt)))
  (if (and (> wv 0) (>= w fl))
    (begin
      (format #t "~a~&" wv)
      (if (<= wv ve)
        (set! wv (- wv))
        (set! wv (* (- loss 1) wv)))))
  (post-redisplay))

(define (on-display)
  (gl-clear (clear-buffer-mask color-buffer))
  (gl-begin (begin-mode lines)
    (gl-color 0 1 0)
    (gl-vertex 320 b)
    (gl-vertex 320 w)
    (gl-vertex 0 fl)
    (gl-vertex 640 fl))
  (glPointSize 5)
  (gl-begin (begin-mode points)
    (gl-color 1 0 0)
    (gl-vertex 320 b)
    (gl-vertex 320 (- w l))
    (gl-vertex 320 w))
  (swap-buffers))

(define (on-reshape width height)
  (pk 'reshape! width height)
  (gl-viewport 0 0 width height)
  (set-gl-matrix-mode (matrix-mode projection))
  (gl-load-identity)
  (gl-ortho 0 width height 0 -1 +1))

(initialize-glut (program-arguments) #:window-size '(640 . 480) #:display-mode (display-mode rgb double))
(set! main-window (make-window "spring"))
(set-display-callback on-display)
(set-reshape-callback on-reshape)
(set-idle-callback     on-idle)
(set-gl-clear-color 0 0 0 1)
(set-gl-matrix-mode (matrix-mode modelview))
(gl-load-identity)
(glut-main-loop)
